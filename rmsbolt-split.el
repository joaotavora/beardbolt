;;; beardbolt-split.el --- An Elisp library to edit command lines -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jay Kamat
;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Version: 0.1.0
;; Keywords: compilation, tools
;; URL: http://gitlab.com/jgkamat/beardbolt
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a small library for editing command line parameters, passed as
;; a string. For example, given:
;; gcc -o one two -c -S
;; This library tries to make it possible to remove '-o one' '-c' and '-S'.
;;
;; This is not easy to do without bugs, and therefore this is mostly incomplete.
;; Hopefully we will never see more complicated formats (such as quoted arguments)
;; in generated commands, or we will need to write a parser.

;;; Requires:

(require 'cl-lib)

;;; Variables

(defvar beardbolt-split--regexp (rx (1+ blank)))

;;; Code:

(defun beardbolt-split-rm-single (cmd flag &optional comparator)
  "Remove a single FLAG from CMD.

Optionally compares using COMPARATOR."
  (let ((cmd (split-string cmd beardbolt-split--regexp))
        (comparator (or comparator #'string=)))
    (mapconcat
     #'identity
     (cl-remove-if (apply-partially comparator flag) cmd)
     " ")))

(defun beardbolt-split-rm-double (cmd flag)
  "Remove a single FLAG and arg from CMD."
  (let ((cmd (split-string cmd beardbolt-split--regexp))
        (removed nil))
    (mapconcat
     #'identity
     (cl-remove-if (lambda (f)
                     (cond
                      ((string= f flag)
                       (setq removed t))
                      (removed
                       (setq removed nil)
                       t)
                      (t nil)))
                   cmd)
     " ")))

(provide 'beardbolt-split)

;;; beardbolt-split.el ends here
