;;; beardbolt-java.el --- An Elisp library to parse javap output -*- lexical-binding: t; -*-

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

;; The java bytecode disassembler format is rather obtuse. This library tries
;; to make a programatic layer for interacting with it. It's main aim is
;; correlating lines in source code to the generated output.
;;
;; This library takes in the output of `javap -c -l` split into a list by lines,
;; which is the same format beardbolt uses.

;;; Requires:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;;; Code:

;;;; Regexes
(defvar beardbolt-java-code-start  (rx bol (1+ space)
                                     (group "Code:")))
(defvar beardbolt-java-line-table-start  (rx bol (1+ space)
                                           (group "LineNumberTable:")))
(defvar beardbolt-java-local-table-start  (rx bol (1+ space)
                                            (group "LocalVariableTable:")))
(defvar beardbolt-java-code (rx bol (group (1+ space)) (group (1+ digit))
                              ":" (1+ space) (group (1+ any)) eol))
(defvar beardbolt-java-line-table (rx bol (1+ space) "line" (1+ space) (group (1+ digit))
                                    ":" (1+ space) (group (1+ digit))))

;;;; Functions
(defun beardbolt-java-process-bytecode (asm-lines &optional filter)
  "Process ASM-LINES to add properties refrencing the source code.
Also FILTER \"useless\" lines out, optionally."
  (let (result state result-hold  code-block code-linum in-bracket)
    (dolist (line asm-lines)
      (pcase state
        ('nil ;; We haven't found any special blocks, so look for them and copy to output
         (when (string-match-p beardbolt-java-code-start line)
           (setq state 'code-found)
           (push line result)))
        ('code-found ;; We are past Code: so begin parsing instructions
         (if (string-match-p beardbolt-java-line-table-start line)
             (setq state 'linum-found)
           (if (and (string-match beardbolt-java-code line)
                    (match-string 1 line)
                    (match-string 2 line)
                    (match-string 3 line)
                    (not in-bracket))
               (progn (push (cons (string-to-number (match-string 2 line))
                                  line)
                            code-block)
                      (when (string-match-p "{" line)
                        (setq in-bracket t)))
             ;; Assume we have a continuation line
             (push (cons (cl-first (car code-block))
                         line)
                   code-block)
             (when (string-match-p "}" line)
               (setq in-bracket nil))
             )))
        ('linum-found ;; We are past LineNumberTable, so begin generating the src->code table
         (if (string-match-p beardbolt-java-local-table-start line)
             (progn
               (setq state 'localvar-found)
               ;; Get everything ready for agg
               (setq code-block (nreverse code-block))
               (setq code-linum (nreverse code-linum)))

           (if (and (string-match beardbolt-java-line-table line)
                    (match-string 1 line)
                    (match-string 2 line))
               (push (cons (string-to-number (match-string 2 line))
                           (string-to-number (match-string 1 line)))
                     code-linum)
             (error "Unexpected output inside LineNumberTable: block of javap"))))
        ('localvar-found ;; Agg results if they exist
         ;; TODO can we assume there is an empty line after LocalVar?
         (if (string-empty-p line)
             (setq state nil)
           (when (and code-linum code-block)
             (let (current-mapping current-line)
               (dolist (l code-block)
                 (when (and code-linum
                            (>= (car l)
                                (car (cl-first code-linum))))
                   ;; We are at (or passed) the line at the top of code-linum mapping, let's use the mapping
                   (setq current-mapping (pop code-linum)))
                 (setq current-line (cdr l))
                 (when (and current-mapping
                            (numberp (cdr current-mapping)))
                   (add-text-properties 0 (length current-line)
                                        `(beardbolt-src-line ,(cdr current-mapping)) current-line))
                 (push current-line result)))
             ;; Don't keep agging
             (setq code-linum nil
                   code-block nil)))))
      (if (not state)
          (progn
            (when result-hold
              ;; We have leftovers in result-hold, let's flush them
              (setq result (append result-hold result))
              (setq result-hold nil))
            (push line result))
        (when (and (not filter)
                   ;; Never output code, that's handled above.
                   ;; Code: is handled on transition
                   (not (eq state 'code-found)))
          (push line result-hold))))
    (nreverse result)))

(provide 'beardbolt-java)

;;; beardbolt-java.el ends here
