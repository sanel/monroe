;;; monroe.el --- Yet another client for nREPL

;; Copyright (c) 2014 Sanel Zukan
;;
;; Author: Sanel Zukan <sanelz@gmail.com>
;; URL: http://www.github.com/sanel/monroe
;; Version: 0.3.0
;; Keywords: languages, clojure, nrepl, lisp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides yet another elisp client to connect to Clojure nREPL servers.

;;; Installation:

;; Copy it to your load-path and run with:
;; M-: (require 'monroe)

;;; Usage:

;; M-x monroe

;;; Code:

(require 'comint)

(defgroup monroe nil
  "Interaction with the nREPL Server."
  :prefix "monroe-"
  :group 'applications)

(defcustom monroe-repl-prompt-format "%s=> "
  "String used for displaying prompt. '%s' is used as
placeholder for storing current namespace."
  :type 'string
  :group 'monroe)

(defcustom monroe-prompt-regexp "^[^> \n]*>+:? *"
  "Regexp to recognize prompts in Monroe more. The same regexp is
used in inferior-lisp."
  :type 'regexp
  :group 'monroe)

(defcustom monroe-default-host "localhost:7888"
  "Default location where to connect to, unless explicitly given
location and port. Location and port should be delimited with ':'."
  :type 'string
  :group 'monroe)

(defcustom monroe-detail-stacktraces nil
  "If set to true, Monroe will try to get full stacktrace from thrown
exception. Otherwise will just behave as standard REPL version."
  :type 'boolean
  :group 'monroe)

(defvar monroe-version "0.3.0"
  "The current monroe version.")

(defvar monroe-session nil
  "Current nREPL session id.")

(defvar monroe-requests (make-hash-table :test 'equal)
  "Map of requests to be processed.")

(defvar monroe-requests-counter 0
  "Serial number for message.")

(defvar monroe-repl-buffer "*monroe*"
  "Name of nREPL buffer.")

(defvar monroe-buffer-ns "user"
  "Current clojure namespace for this buffer. This namespace
is only advertised until first expression is evaluated, then is updated
to the one used on nrepl side.")

(make-variable-buffer-local 'monroe-session)
(make-variable-buffer-local 'monroe-requests)
(make-variable-buffer-local 'monroe-requests-counter)
(make-variable-buffer-local 'monroe-buffer-ns)

;;; message stuff

;; Idea for message handling (via callbacks) and destructuring response is shamelessly
;; stolen from nrepl.el.
(defmacro monroe-dbind-response (response keys &rest body)
  "Destructure an nREPL response dict."
  `(let ,(loop for key in keys
			   collect `(,key (cdr (assoc ,(format "%s" key) ,response))))
	 ,@body))

;;; Bencode
;;; Stolen from nrepl.el which is adapted from http://www.emacswiki.org/emacs-en/bencode.el
(defun monroe-bdecode-buffer ()
  "Decode a bencoded string in the current buffer starting at point."
  (cond
   ((looking-at "i\\([0-9]+\\)e")
	(goto-char (match-end 0))
	(string-to-number (match-string 1)))
   ((looking-at "\\([0-9]+\\):")
	(goto-char (match-end 0))
	(let* ((start (point))
		   (end (byte-to-position (+ (position-bytes start) (string-to-number (match-string 1))))))
	  (goto-char end)
	  (buffer-substring-no-properties start end)))
   ((looking-at "l")
	(goto-char (match-end 0))
	(let (result item)
	  (while (setq item (monroe-bdecode-buffer))
		(setq result (cons item result)))
	  (nreverse result)))
   ((looking-at "d")
	(goto-char (match-end 0))
	(let (dict key item)
	  (while (setq item (monroe-bdecode-buffer))
		(if key
			(setq dict (cons (cons key item) dict)
				  key nil)
		  (unless (stringp item)
			(error "Dictionary keys have to be strings: %s" item))
		  (setq key item)))
	  (cons 'dict (nreverse dict))))
   ((looking-at "e")
	(goto-char (match-end 0))
	nil)
   (t
	(error "Cannot decode object: %d" (point)))))

(defun monroe-encode (message)
  "Encode message to nrepl format. The message format is
'd<key-len>:key<val-len>:value<key-len>:key<val-len>:valuee', where the message is
starting with 'd' and ending with 'e'."
  (concat "d"
	(apply 'concat
	  (mapcar (lambda (str)
				(format "%d:%s" (string-bytes str) str))
			  message))
	"e"))

(defun monroe-decode (str)
  "Decode message using temporary buffer."
  (with-temp-buffer
	(save-excursion (insert str))
	(let ((result '()))
	  (while (not (eobp))
		(setq result (cons (monroe-bdecode-buffer) result)))
	  (nreverse result))))

(defun monroe-write-message (process message)
  "Send message to given process."
  (process-send-string process message))

(defun monroe-send-request (request callback)
  "Send request as elisp object and assign callback to
be called when reply is received."
  (let* ((id       (number-to-string (incf monroe-requests-counter)))
		 (message  (append (list "id" id) request))
		 (bmessage (monroe-encode message)))
	(puthash id callback monroe-requests)
	(monroe-write-message "*monroe-connection*" bmessage)))

(defun monroe-clear-request-table ()
  "Erases current request table."
  (clrhash monroe-requests)
  (setq monroe-requests-counter 0))

(defun monroe-current-session ()
  "Returns current session id."
  (with-current-buffer "*monroe-connection*"
	monroe-session))

;;; nrepl messages we knows about

(defun monroe-send-hello (callback)
  "Initiate nREPL session."
  (monroe-send-request '("op" "clone") callback))

(defun monroe-send-describe (callback)
  "Produce a machine- and human-readable directory and documentation for
the operations supported by an nREPL endpoint."
  (monroe-send-request '("op" "describe") callback))

(defun monroe-send-eval-string (str callback)
  "Send code for evaluation on given namespace."
  (monroe-send-request (list "op" "eval"
							 "session" (monroe-current-session)
							 "code" str)
					   callback))

(defun monroe-send-stdin (str callback)
  "Send stdin value."
  (monroe-send-request (list "op" "stdin"
							 "session" (monroe-current-session)
							 "stdin" str)
					   callback))

(defun monroe-send-interrupt (request-id callback)
  "Send interrupt for pending requests."
  (monroe-send-request (list "op" "interrupt"
							 "session" (monroe-current-session)
							 "interrupt-id" request-id)
					   callback))

;;; code

(defun monroe-make-response-handler ()
  "Returns a function that will be called when event is received."
   (lambda (response)
	 (monroe-dbind-response response (id ns value err out ex root-ex status)
	   (let ((output (concat err out
							 (if value
							   (concat value "\n"))))
			 (process (get-buffer-process monroe-repl-buffer)))
		 ;; update namespace if needed
		 (if ns (setq monroe-buffer-ns ns))
		 (comint-output-filter process output)
		 ;; now handle status
		 (when status
		   (when (and monroe-detail-stacktraces (member "eval-error" status))
			 (monroe-get-stacktrace root-ex ex))
		   (when (member "interrupted" status)
			 (message "Evaluation interrupted."))
		   (when (member "need-input" status)
			 (monroe-handle-input))
		   (when (member "done" status)
			 (remhash id monroe-requests)))
		 ;; show prompt only when no output is given in any of received vars
		 (unless (or err out value root-ex ex)
		   (comint-output-filter process (format monroe-repl-prompt-format monroe-buffer-ns)))))))

(defun monroe-input-sender (proc input)
  "Called when user enter data in REPL and when something is received in."
  (monroe-send-eval-string input (monroe-make-response-handler)))

(defun monroe-input-sender-with-history (proc input)
  "Called when user enter data in REPL. It will also record input for
history purposes."
  (comint-add-to-input-history input)
  (monroe-input-sender proc input))

(defun monroe-handle-input ()
  "Called when requested user input."
  (monroe-send-stdin
   (concat (read-from-minibuffer "Stdin: ") "\n")
   (monroe-make-response-handler)))

(defun monroe-sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (message "nREPL connection closed: %s" message)
  (kill-buffer (process-buffer process))
  (monroe-disconnect))

(defun monroe-dispatch (response)
  "Find response id and call associated callback."
  (monroe-dbind-response response (id)
	(let ((callback (gethash id monroe-requests)))
	  (when callback
		(funcall callback response)))))

(defun monroe-net-decode ()
  "Decode the data in the current buffer and remove the processed data from the
buffer if the decode successful."
  (let* ((start   (point-min))
		 (end     (point-max))
		 (data    (buffer-substring start end))
		 (decoded (monroe-decode data)))
	(delete-region start end)
	decoded))

(defun monroe-net-filter (process string)
  "Called when the new message is received. Process will redirect
all received output to this function; it will decode it and put in
monroe-repl-buffer."
  (with-current-buffer (process-buffer process)
	(goto-char (point-max))
	(insert string)
	;; Stolen from Cider. Assure we have end of the message so decoding can work;
	;; to make sure we are at the real end (session id can contain 'e' character), we call
	;; 'accept-process-output' once more.
	;;
	;; This 'ignore-errors' is a hard hack here since 'accept-process-output' will call filter
	;; which will be this function causing Emacs to hit max stack size limit.
	(ignore-errors
	  (when (eq ?e (aref string (- (length string) 1)))
		(unless (accept-process-output process 0.01)
		  (while (> (buffer-size) 1)
			(dolist (response (monroe-net-decode))
			  (monroe-dispatch response))))))))

(defun monroe-new-session-handler (process)
  "Returns callback that is called when new connection is established."
  (lambda (response)
	(monroe-dbind-response response (id new-session)
	  (when new-session
		(message "Connected.")
		(setq monroe-session new-session)
		(remhash id monroe-requests)))))

(defun monroe-valid-host-string (str default)
  "Used for getting valid string for host/port part."
  (if (and str (not (string= "" str)))
	str
	default))

(defun monroe-connect (host-and-port)
  "Connect to remote endpoint using provided hostname and port."
  (let* ((hp   (split-string host-and-port ":"))
		 (host (monroe-valid-host-string (first hp) "localhost"))
		 (port (monroe-valid-host-string (second hp) "7888")))
	(message "Connecting to nREPL host on '%s:%s'..." host port)
	(let ((process (open-network-stream "monroe" "*monroe-connection*" host port)))
	  (set-process-filter process 'monroe-net-filter)
	  (set-process-sentinel process 'monroe-sentinel)
	  (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
	  (monroe-send-hello (monroe-new-session-handler (process-buffer process)))
	  process)))

(defun monroe-disconnect ()
  "Disconnect from current nrepl connection. Calling this function directly
will force connection closing, which will as result call '(monroe-sentinel)'."
  (monroe-clear-request-table)
  (let ((delete-process-safe (lambda (p)
							   (when (and p (process-live-p p))
								 (delete-process p))))
		;; 'monroe-repl-buffer' process is actually 'fake-proc'
		(proc1 (get-buffer-process monroe-repl-buffer))
		(proc2 (get-buffer-process "*monroe-connection*")))
	(funcall delete-process-safe proc1)
	(funcall delete-process-safe proc2)))

;;; keys

(defun monroe-eval-region (start end)
  "Evaluate selected region."
  (interactive "r")
  (monroe-input-sender (get-buffer-process monroe-repl-buffer) (buffer-substring-no-properties start end)))

(defun monroe-eval-buffer ()
  "Evaluate the buffer."
  (interactive)
  (monroe-eval-region (point-min) (point-max)))

(defun monroe-eval-expression-at-point ()
  "Figure out expression at point and send it for evaluation."
  (interactive)
  (save-excursion
	(end-of-defun)
	(let ((end (point)))
	  (beginning-of-defun)
	  (monroe-eval-region (point) end))))

(defun monroe-eval-namespace ()
  "Tries to evaluate Clojure ns form. It does this by matching first
expression at the beginning of the file and evaluating it. Not something
that is 100% accurate, but Clojure practice is to keep ns forms always
at the top of the file."
  (interactive)
  (when (and (fboundp 'clojure-find-ns)
			 (funcall 'clojure-find-ns))
	(save-excursion
	  (goto-char (match-beginning 0))
	  (monroe-eval-expression-at-point))))

(defun monroe-eval-doc (symbol)
  "Internal function to actually ask for symbol documentation via nrepl protocol."
  (monroe-input-sender (get-buffer-process monroe-repl-buffer) (format "(clojure.repl/doc %s)" symbol)))

(defun monroe-get-stacktrace (root-ex ex)
  "When error is happened, try to get as much details as possible from last stracktrace."
  (monroe-input-sender
   (get-buffer-process monroe-repl-buffer)
   "(if-let [pst+ (resolve 'clj-stacktrace.repl/pst)]
      (pst+ *e)
      (clojure.stacktrace/print-stack-trace *e))"))

(defun monroe-describe (symbol)
  "Ask user about symbol and show symbol documentation if found."
  (interactive
   (list
	(let* ((sym (thing-at-point 'symbol))
		   (sym (if sym (substring-no-properties sym)))
		   (prompt "Describe")
		   (prompt (if sym
				     (format "%s (default %s): " prompt sym)
					 (concat prompt ": "))))
	  (read-string prompt nil nil sym))))
  (monroe-eval-doc symbol))

(defun monroe-load-file (path)
  "Load file to running process, asking user for alternative path.
This function, contrary to clojure-mode.el, will not use comint-mode for sending files
as path can be remote location. For remote paths, use absolute path."
  (interactive
   (list
	(let ((n (buffer-file-name)))
	  (read-file-name "Load file: " nil nil nil
					  (and n (file-name-nondirectory n))))))
  (let ((full-path (convert-standard-filename (expand-file-name path))))
	(monroe-input-sender
	 (get-buffer-process monroe-repl-buffer)
	 (format "(clojure.core/load-file \"%s\")" full-path))))

(defun monroe-extract-keys (htable)
  "Get all keys from hashtable."
  (let (keys)
	(maphash (lambda (k v) (setq keys (cons k keys))) htable)
	keys))

(defun monroe-interrupt ()
  "Send interrupt to all pending requests."
  (interactive)
  (dolist (id (monroe-extract-keys monroe-requests))
	(monroe-send-interrupt id (monroe-make-response-handler))))

;; keys for interacting with Monroe REPL buffer
(defvar monroe-interaction-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "\C-c\C-c" 'monroe-eval-expression-at-point)
	(define-key map "\C-c\C-r" 'monroe-eval-region)
	(define-key map "\C-c\C-k" 'monroe-eval-buffer)
	(define-key map "\C-c\C-n" 'monroe-eval-namespace)
	(define-key map "\C-c\C-d" 'monroe-describe)
	(define-key map "\C-c\C-b" 'monroe-interrupt)
	(define-key map "\C-c\C-l" 'monroe-load-file)
	map))

;; keys for interacting inside Monroe REPL buffer
(defvar monroe-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
	(define-key map "\C-c\C-d" 'monroe-describe)
	(define-key map "\C-c\C-c" 'monroe-interrupt)
	map))

;;; rest

(define-derived-mode monroe-mode comint-mode "Monroe nREPL"
  "Major mode for evaluating commands over nREPL.

The following keys are available in `monroe-mode':

  \\{monroe-mode-map}"

  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-regexp monroe-prompt-regexp)
  (setq comint-input-sender 'monroe-input-sender-with-history)
  (setq mode-line-process '(":%s"))
  ;(set (make-local-variable 'font-lock-defaults) '(clojure-font-lock-keywords t))

  ;; a hack to keep comint happy
  (unless (comint-check-proc (current-buffer))
	(let ((fake-proc (start-process "monroe" (current-buffer) nil)))
	  (set-process-query-on-exit-flag fake-proc nil)
	  (insert (format ";; Monroe nREPL %s\n" monroe-version))
	  (set-marker (process-mark fake-proc) (point))
	  (comint-output-filter fake-proc (format monroe-repl-prompt-format monroe-buffer-ns)))))

;;; user command

(defun clojure-enable-monroe ()
  (monroe-interaction-mode t))

;;;###autoload
(define-minor-mode monroe-interaction-mode
  "Minor mode for Monroe interaction from a Clojure buffer.

The following keys are available in `monroe-interaction-mode`:

  \\{monroe-interaction-mode}"

  nil " Monroe" monroe-interaction-mode-map)

;;;###autoload
(defun monroe (host-and-port)
  "Load monroe by setting up appropriate mode, asking user for
connection endpoint."
  (interactive
   (list
	(read-string (format "Host (default '%s'): " monroe-default-host)
				 nil nil monroe-default-host)))
  (unless (ignore-errors
			(with-current-buffer (get-buffer-create monroe-repl-buffer)
			  (prog1
				  (monroe-connect host-and-port)
				(goto-char (point-max))
				(monroe-mode)
				(switch-to-buffer (current-buffer)))))
	(message "Unable to connect to %s." host-and-port)))

(provide 'monroe)

;;; monroe.el ends here
