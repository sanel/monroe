# monroe.el --- Yet another client for nREPL
#
# Copyright (c) 2014-2018 Sanel Zukan
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

.PHONY: clean compile

EMACS  ?= emacs
FILES  ?= monroe.el
STRICT ?= --eval '(setq byte-compile-error-on-warn t)'

%.elc:%.el
	$(EMACS) --batch -Q -L . $(STRICT) -f batch-byte-compile $<

compile: $(FILES:.el=.elc)

all: compile

clean:
	$(RM) *.elc
