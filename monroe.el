;;; monroe.el --- Yet another client for nREPL

;; Copyright (c) 2014 Sanel Zukan
;;
;; Author: Sanel Zukan <sanelz@gmail.com>
;; URL: http://www.github.com/sanel/monroe
;; Version: 0.1.0
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

;; Just copy it to your load-path and run with:
;; M-: (require 'monroe)

;;; Usage:

;; M-x monroe

;;; Code:

(require 'comint)
(require 'inf-lisp)

(defgroup monroe nil
  "Interaction with the nREPL Server."
  :prefix "monroe-"
  :group 'applications)

(defvar monroe-version "0.1.0"
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
  "Current clojure namespace for this buffer.")

(defvar monroe-connection-process nil
  "Current connection object to nREPL server. For internal usage.")

(defcustom monroe-prompt-regexp "^[^> \n]*>+:? *"
  "Regexp to recognize prompts in Monroe more. The same regexp is
used in inferior-lisp."
  :type 'regexp
  :group 'monroe)

(defvar monroe-fake-proc nil
  "For storing global fake proc. Since we are aiming to support older Emacs versions,
lexical binding is not used; this also disables closures.")

;;; message stuff

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
	(let ((start (point))
		  (end (byte-to-position (+ (position-bytes (point)) (string-to-number (match-string 1))))))
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
	;(message (format "-> %s|%s|" message bmessage))
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
;;; code

(defun monroe-input-sender (proc input)
  (comint-add-to-input-history input)
  ;(message "i-> |%s|" input)
  (monroe-send-eval-string input
   (lambda (response)
	 (monroe-dbind-response response (id ns value err out)
       ;; we can accept also 'ex' and 'root-ex' variables, but
       ;; for now ignoring them; 'out' will contain full stacktrace
	   (let ((output
			  (apply #'concat
					 (mapcar (lambda (v) (if v (concat v "\n")))
							 (list err out value)))))
		 ;; update namespace if needed
		 (if ns (setq monroe-buffer-ns ns))
		 (comint-output-filter monroe-fake-proc output)
		 (comint-output-filter monroe-fake-proc (format "%s=> " monroe-buffer-ns)))))))

(defun monroe-sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (message "nREPL connection closed: %s" message)
  (kill-buffer (process-buffer process))
  (monroe-disconnect))

(defun monroe-dispatch (response)
  "Find response id and call associated callback."
  ;(message "Calling dispatch on %s" response)
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
	;(message (format "<d %s|%s|" data decoded))
	(delete-region start end)
	decoded))

(defun monroe-net-filter (process string)
  "Called when the new message is received. Process will redirect
all received output to this function; it will decode it and put in
monroe-repl-buffer."
  ;(message (format "<- %s\n" string))
  (with-current-buffer (process-buffer process)
	(goto-char (point-max))
	(insert string)
	(while (> (buffer-size) 1)
	  (dolist (response (monroe-net-decode))
		(monroe-dispatch response)))))

(defun monroe-new-session-handler (process)
  "Returns callback that is called when new connection is established."
  (lambda (response)
	(monroe-dbind-response response (id new-session)
      (when new-session
		(message "Connected.")
		(setq monroe-session new-session)
		(remhash id monroe-requests)))))

(defun monroe-connect (host port)
  "Connect to remote endpoint using provided hostname and port."
  (message "Connecting to nREPL host on '%s:%s'..." host port)
  (let ((process (open-network-stream "monroe" "*monroe-connection*" host port)))
	(set-process-filter process 'monroe-net-filter)
	(set-process-sentinel process 'monroe-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
	(monroe-send-hello (monroe-new-session-handler (process-buffer process)))
	process))

(defun monroe-disconnect ()
  "Disconnect from current nrepl connection."
  (when monroe-connection-process
	(monroe-clear-request-table)
	(delete-process monroe-connection-process)
	(setq monroe-connection-process nil)
	(delete-process monroe-fake-proc)
	(setq monroe-fake-proc nil)))

(defun monroe-input-filter-function (str)
  (message "--->%s\n" str)
  "boo")

(define-derived-mode monroe-mode comint-mode "Monroe nREPL"
  "Major mode for evaluating commands over nREPL."
  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-regexp monroe-prompt-regexp)
  (setq comint-input-sender 'monroe-input-sender)
  (setq mode-line-process '(":%s"))
  
  ;; a hack to keep comint happy
  (unless (comint-check-proc (current-buffer))
	(let ((fake-proc
		   (condition-case nil
			   (start-process "monroe" (current-buffer) nil)
			 (file-error (start-process "monroe" (current-buffer) nil)))))
	  (set-process-query-on-exit-flag fake-proc nil)
	  (insert (format ";; Monroe nREPL %s\n" monroe-version))
	  (set-marker (process-mark fake-proc) (point))
	  (comint-output-filter fake-proc (format "%s=> " monroe-buffer-ns))
	  ;(add-hook 'comint-input-filter-functions 'monroe-input-filter-function nil t)
	  (setq monroe-fake-proc fake-proc))))

;;; user command

;;;###autoload
(defun monroe (host port)
  "Load monroe by setting up appropriate mode, asking user for
connection endpoint."
  (interactive "sHost: \nnPort: ")
  (setq monroe-connection-process
		(ignore-errors
		  (with-current-buffer (get-buffer-create monroe-repl-buffer)
			(prog1
				(monroe-connect host port)
			  (monroe-mode)
			  (switch-to-buffer monroe-repl-buffer)
			  (setq inferior-lisp-buffer monroe-repl-buffer)))))
  (unless monroe-connection-process
	(message "Unable to connect to %s:%s." host port)))

(provide 'monroe)
