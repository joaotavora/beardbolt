;;; beardbolt.el --- A compiler output viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021 Jay Kamat 2022 João Távora
;; Author: João Távora <joaotavora@gmail.com>
;; Version: 0.1.2
;; Keywords: compilation, tools
;; URL: https://github.com/joaotavora/beardbolt
;; Package-Requires: ((emacs "28.1"))

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

;; beardbolt is a fork of the amazing rmsbolt.el, found at
;; https://gitlab.com/jgkamat/rmsbolt, a package to provide assembly or
;; bytecode output for a source code input file.


;;; Requires:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'map)
(require 'compile)
(require 'disass)
(require 'json)
(require 'color)
(require 'pulse)

;;; Code:
(defgroup beardbolt nil
  "beardbolt customization options"
  :group 'applications)

(defmacro bb--defoption (sym &rest whatever)
  `(progn (defcustom ,sym ,@whatever) (put ',sym 'bb--option t)))

(bb--defoption bb-command nil
  "The base command to run beardbolt from."
  :type 'string :safe (lambda (v) (or (listp v) (stringp v))))
(bb--defoption bb-disassemble nil
  "Non-nil to assemble then disassemble an output binary."
  :type 'boolean :safe 'booleanp)
(bb--defoption bb-asm-format 'att
  "Which output assembly format to use.
Passed directly to compiler or disassembler."
  :type 'string :safe (lambda (v) (or (null v) (symbolp v) (stringp v))))
(bb--defoption bb-preserve-directives nil
  "Non-nil to keep assembly directives."
  :type 'boolean :safe 'booleanp)
(bb--defoption bb-preserve-unused-labels nil
  "Non-nil to keep unused labels."
  :type 'boolean :safe 'booleanp)
(bb--defoption bb-preserve-library-functions nil
  "Non-nil to keep functions with no code related to current file."
  :type 'boolean :safe 'booleanp)
(bb--defoption bb-preserve-comments nil
  "Non-nil to filter comment-only lines."
  :type 'boolean :safe 'booleanp)
(bb--defoption bb-demangle t
  "Non-nil to attempt to demangle the resulting assembly."
  :type 'boolean :safe 'booleanp)
(bb--defoption bb-execute nil
  "Non-nil to run resulting program with these arguments."
  :type 'string :safe (lambda (v) (or (null v) (eq t v) (stringp v))))
(bb--defoption bb-ccj-extra-flags nil
  "Extra flags for compilation command devined from compile_commands.json."
  :type 'string :safe (lambda (v) (or (null v) (stringp v))))
(bb--defoption bb-shuffle-rainbow nil
  "Choose less pretty, but potentially more contrasting rainbow colors."
  :type 'boolean :safe 'booleanp)

(defface bb-current-line-face
  '((t (:weight bold :inherit highlight)))
  "Face to fontify the current line for showing matches.")

(defvar-local bb--asm-buffer nil)
(defvar-local bb--source-buffer nil)
(defvar-local bb--compile-spec nil)
(defvar-local bb--declared-output nil)
(defvar-local bb--dump-file nil "Temporary file")
(defvar-local bb--line-mappings nil
  "List where of asm-to-source mappings.
Each element is ((ASM-BEG-LINE . ASM-END-LINE) . SRC-LINE).")
(defvar-local bb--rainbow-overlays nil "Rainbow overlays.")

(defun bb--asm-buffer (src-buffer)
  "Get/create asm buffer for current source file."
  (with-current-buffer src-buffer
    (or (and (buffer-live-p bb--asm-buffer)
             (equal (buffer-name bb--asm-buffer) "*bb-asm*")
             bb--asm-buffer)
        (setq bb--asm-buffer
              (with-current-buffer
                  (get-buffer-create "*bb-asm*")
                (current-buffer))))))

(defvar bb-compile-delay 0.6
  "Time in seconds to delay before recompiling if there is a change.
If nil, auto-recompilation is off.")

(defvar bb--shell "bash"
  "Which shell to prefer if available.
Used to work around inconsistencies in alternative shells.")

(defun bb--sandbox-dir ()
  (let ((d (expand-file-name "beardbolt-sandbox" user-emacs-directory)))
    (make-directory d 'parents)
    d))

(defvar bb-dir (file-name-directory load-file-name)
  "The directory which beardbolt is installed to.")

(defvar-local bb-objdump-binary "objdump"
  "A binary to use for objdumping when using `bb-disassemble'.
Useful if you have multiple objdumpers and want to select between them")

;;;; Regexes

(defvar bb-label-start  "^\\([^:]+\\): *\\(?:#\\|$\\)\\(?:.*\\)")


(defvar bb-defines-global (rx bol (0+ space) ".glob"
                              (opt "a") "l" (0+ space)
                              (group (any ".a-zA-Z_")
                                     (0+ (any "a-zA-Z0-9$_.")))))
(defvar bb-label-reference (rx "." (any "a-zA-Z_")
                               (0+
                                (any "a-zA-Z0-9$_."))))
(defvar bb-has-opcode (rx bol (1+ space)
                          (1+ (any "a-zA-Z"))))

(defvar bb-defines-function-or-object (rx bol
                                          (0+ space) ".type"
                                          (0+ space)
                                          (group (0+ any)) "," (0+ space) (any "@%")))
(defvar bb-data-defn (rx bol (0+ space) "."
                         (group (or "string" "asciz" "ascii"
                                    (and
                                     (optional (any "1248")) "byte")
                                    "short" "word" "long" "quad" "value" "zero"))))

(defvar bb-endblock (rx "." (or "cfi_endproc" "data" "text" "section")))
(defvar bb-comment-only (rx bol (0+ space) (or (and (or (any "#@;") "//"))
                                               (and "/*" (0+ any) "*/"))
                            (0+ any) eol))
(defvar bb-disass-line (rx bol
                           (group "/" (1+ (not (any ":")))) ":"
                           (group (1+ num))
                           (0+ any)))
(defvar bb-disass-label (rx bol (group (1+ (any digit "a-f")))
                            (1+ space) "<"
                            (group (1+ (not (any ">")))) ">:" eol))
(defvar bb-disass-dest (rx (0+ any) (group (1+ (any digit "a-f")))
                           (1+ space) "<" (group (1+ (not (any ">")))) ">" eol))

(defvar bb-disass-opcode (rx bol (0+ space) (group (1+ (any digit "a-f")))
                             ":" (0+ space)
                             (group (1+
                                     (repeat 2
                                             (any digit "a-f"))
                                     (opt " ")))
                             (0+ space)
                             (group (0+ any))))
(defvar bb-source-file-hint (rx bol (0+ space) ".file" (1+ space)
                                (group (1+ digit)) (1+ space) ?\"
                                (group (1+ (not (any ?\")))) ?\"
                                (opt (1+ space) ?\"
                                     (group (1+ (not (any ?\")))) ?\")
                                (0+ any)))
(defvar bb-source-tag (rx bol (0+ space) ".loc" (1+ space)
                          (group (1+ digit)) (1+ space)
                          (group (1+ digit))
                          (0+ any)))
(defvar bb-source-stab (rx bol (0+ any) ".stabn" (1+ space)
                           (group (1+ digit)) ",0,"
                           (group (1+ digit)) "," (0+ any)))

(defun bb--split-rm-single (cmd flag &optional test)
  "Remove a single FLAG from CMD.  Test according to TEST."
  (mapconcat #'identity (cl-remove flag (split-string cmd)
                                   :test (or test #'string=))
             " "))

(defun bb--split-rm-double (cmd flag)
  "Remove a FLAG and subsequent arg from CMD."
  (cl-loop while split with split = (split-string cmd)
           for i from 0
           for probe = (car split)
           if (string= probe flag) do (setq split (cddr split))
           else
           concat (and (cl-plusp i) " ")
           and concat probe and do (setq split (cdr split))))

(cl-defun bb--c/c++-setup (&key base-cmd language)
  "Get compile specs for gcc/clang."
  (let* ((modified-p (buffer-modified-p))
         (source-hint (if modified-p "<stdin>" (buffer-file-name)))
         (base-command (ensure-list (or bb-command
                                        (bb--guess-from-ccj)
                                        base-cmd)))
         (cc (car (split-string (car base-command)))))
    (cl-labels ((f (x) (expand-file-name x (bb--sandbox-dir)))
                (compile (in out) `(,@base-command
                                    "-g1"
                                    "-S" ,(format "-masm=%s" bb-asm-format)
                                    "-o" ,out
                                    ,@(if modified-p
                                          `("-x" ,language "-" "<" ,in)
                                        `(,(buffer-file-name)))))
                (assemble (in out) `("&&" ,cc "-c" ,in "-o" ,out))
                (link     (in out) `("&&" ,cc ,in      "-o" ,out))
                (execute  (in)     `("&& (" ,in
                                     ,(if (stringp bb-execute) bb-execute "")
                                     "|| true )"))
                (disassemble (in out) `("&&" ,bb-objdump-binary "-d"
                                        ,in "--insn-width=16" "-l"
                                        ,(when bb-asm-format (format "-M %s" bb-asm-format))
                                        ">" ,out)))
      `((:compile
         ,(lambda (dump-file)
            (cons
             `(,(compile dump-file (f "beardbolt.s"))
               ,@(when bb-execute
                   `(,(assemble (f "beardbolt.s") (f "beardbolt.o"))
                     ,(link     (f "beardbolt.o") (f "beardbolt.out"))
                     ,(execute  (f "beardbolt.out")))))
             (f "beardbolt.s")))
         ,(lambda (_dump-file) (bb--process-asm source-hint)))
        (:compile-assemble-disassemble
         ,(lambda (dump-file)
            (cons
             `(,(compile     dump-file         (f "beardbolt.s"))
               ,(assemble    (f "beardbolt.s") (f "beardbolt.o"))
               ,(disassemble (f "beardbolt.o") (f "beardbolt.o.disass"))
               ,@(when bb-execute
                   `(,(link    (f "beardbolt.o") (f "beardbolt.out"))
                     ,(execute (f "beardbolt.out")))))
             (f "beardbolt.o.disass")))
         ,(lambda (_dump-file)
            (bb--process-disassembled-lines source-hint)))))))

(cl-defun bb--rust-setup () "Get compile specs for rustc"
  (let* ((base (ensure-list (or bb-command "rustc"))))
    (cl-labels ((f (x) (expand-file-name x (bb--sandbox-dir)))
                (disassemble (in out) `("&&" ,bb-objdump-binary "-d"
                                        ,in "--insn-width=16" "-l"
                                        ,(when bb-asm-format (format "-M %s" bb-asm-format))
                                        ">" ,out))
                (link (in out)
                  `(,@base "-C debuginfo=1" "--emit" "link" ,in "-o" ,out))
                (compile (in out)
                  `(,@base "-C debuginfo=1" "--emit" "asm" ,in
                           ,(when bb-asm-format (format
                                                 "-Cllvm-args=--x86-asm-syntax=%s"
                                                 bb-asm-format))
                           "-o" ,out)))
      `((:compile
         ,(lambda (dump-file)
            (cons
             (compile dump-file (f "beardbolt.s"))
             (f "beardbolt.s")))
         ,#'bb--process-asm)
        (:compile-assemble-disassemble
         ,(lambda (dump-file)
            (cons
             `(,(link        dump-file         (f "beardbolt.o"))
               ,(disassemble (f "beardbolt.o") (f "beardbolt.o.disass")))
             (f "beardbolt.o.disass")))
         ,#'bb--process-disassembled-lines)))))

(defvar bb-languages
  `((c-mode    ,#'bb--c/c++-setup :base-cmd "gcc" :language "c")
    (c++-mode  ,#'bb--c/c++-setup :base-cmd "g++" :language "c++")
    (rust-mode ,#'bb--rust-setup))
  "Alist of (MAJOR-MODE SETUP . SETUP-ARGS).

SETUP is a function called with `apply' on SETUP-ARGS.

It returns a list (SPEC ...) where SPEC is (WHAT CMD-FN PRETTY-FN).

WHAT is a symbol `:compile' or `:compile-assemble-disassemble'.

CMD-FN is a function taking IN-FILE, name of the temp file with
the current buffer's content, and returning a cons cell (CMD
. OUT-FILE).  CMD is a string or a tree of strings.  When
flattened and joined with whitespace, it represents a shell
command to produce a file named OUT-FILE, whose contents are
inserted into the asm buffer.

PRETTY-FN is a function taking IN-FILE, to run in the asm buffer
where OUT-FILE's contents are freshly inserted. It may clean up
some parts of the buffer and setup a buffer-local value of
`beardbolt--line-mappings' (which see).")

(defmacro bb--get (sym) `(buffer-local-value ',sym bb--source-buffer))

(defmacro bb--sweeping (&rest forms)
  (declare (indent 0)
           (debug (&rest (form &rest form))))
  (let ((lbp (cl-gensym "lbp-")) (lep (cl-gensym "lep-"))
        (preserve-directives (cl-gensym "preserve-directives-"))
        (linum (cl-gensym "linum-")))
    `(let ((,preserve-directives (bb--get bb-preserve-directives))
           (,linum 1))
       (goto-char (point-min))
       (while (not (eobp))
         (let ((,lbp (line-beginning-position)) (,lep (line-end-position)))
           (cl-macrolet ((match (&rest res)
                           `(cl-loop for re in ,(cons 'list res)
                                     thereis (re-search-forward re ,',lep t)))
                         (update-lep () `(setq ,',lep (line-end-position)))
                         (asm-linum () ',linum)
                         (preserve () `(progn
                                         (forward-line 1)
                                         (setq ,',linum (1+ ,',linum)))))
             (pcase (cond ,@forms)
               (:preserve (preserve))
               (:kill (delete-region ,lbp (1+ ,lep)))
               (_
                (if ,preserve-directives (preserve)
                  (delete-region ,lbp (1+ ,lep)))))))))))

(defun bb--register-mapping (source-linum l)
  (let ((current-chunk (car bb--line-mappings)))
    (if (and (eq source-linum (cdr current-chunk))
             (eq l (1+ (cdar current-chunk))))
        (setf (cdar current-chunk) l)
      (push (cons (cons l l) source-linum)
            bb--line-mappings))))

(cl-defun bb--process-disassembled-lines (main-file-name)
  (let* ((func nil) (source-linum nil))
    (cl-flet ((bb--user-func-p (func)
                (let* ((regexp (rx bol (or (and "__" (0+ any))
                                           (and "_" (or "init" "start" "fini"))
                                           (and (opt "de") "register_tm_clones")
                                           "call_gmon_start"
                                           "frame_dummy"
                                           (and ".plt" (0+ any)))
                                   eol)))
                  (if regexp (not (string-match-p regexp func)) t))))
      (bb--sweeping
        ((match bb-disass-line)
         (setq source-linum (and (equal (file-name-base main-file-name) ;; brittle
                                        (file-name-base (match-string 1)))
                                 (string-to-number (match-string 2))))
         :kill)
        ((match bb-disass-label)
         (setq func (match-string 2))
         (when (bb--user-func-p func) (replace-match (concat func ":")))
         :preserve)
        ((and func (not (bb--user-func-p func)))
         :kill)
        ((match bb-disass-opcode)
         (when source-linum
           (bb--register-mapping source-linum (asm-linum)))
         (replace-match (concat (match-string 1) "\t" (match-string 3)))
         :preserve)))))

(defun bb--process-asm (main-file-name)
  (let* ((used-labels (obarray-make))
         (routines (make-hash-table :test #'equal))
         main-file-tag
         main-file-routines
         source-linum
         current-routine
         reachable-label
         (preserve-comments (bb--get bb-preserve-comments))
         (preserve-unused-labels (bb--get bb-preserve-unused-labels))
         (preserve-library-functions (bb--get bb-preserve-library-functions)))
    (bb--sweeping ; first pass
      ((not (eq (char-after) ?\t))
       (cond ((match bb-label-start)
              (unless (eq :notfound (gethash (match-string 1) routines :notfound))
                (setq current-routine (match-string 1)))
              :preserve)
             (t :kill)))
      (t
       (cond ((and current-routine (match bb-has-opcode))
              (while (match bb-label-reference)
                (push (match-string 0) (gethash current-routine routines)))
              :preserve)
             ((and (not preserve-comments) (match bb-comment-only))
              :kill)
             ((match bb-defines-global bb-defines-function-or-object)
              (puthash (match-string 1) nil routines))
             ((and (match bb-source-file-hint)
                   (equal (or (match-string 3) (match-string 2))
                          main-file-name))
              (setq main-file-tag (match-string 1)))
             ((match bb-source-tag)
              (when (and current-routine
                         (equal (match-string 1) main-file-tag))
                (push current-routine main-file-routines))
              :preserve)
             ((match bb-endblock) (setq current-routine nil) :preserve)
             (t :preserve))))
    (dolist (mfr (if preserve-library-functions
                     (hash-table-keys routines)
                   main-file-routines))
      (intern mfr used-labels)
      (dolist (callee (gethash mfr routines)) (intern callee used-labels)))
    (bb--sweeping ; second pass
      ((not (eq (char-after) ?\t))
       (when (match bb-label-start)
         (cond
          ((intern-soft (match-string 1) used-labels)
           (setq reachable-label (match-string 1))
           :preserve)
          (t
           (if preserve-unused-labels :preserve :kill)))))
      (t
       (cond ((and (match bb-data-defn) reachable-label)
              :preserve)
             ((and (match bb-has-opcode) reachable-label)
              (when source-linum (bb--register-mapping source-linum (asm-linum)))
              :preserve)
             ((match bb-source-tag)
              (setq source-linum
                    (and (equal (match-string 1) main-file-tag)
                         (string-to-number (match-string 2)))))
             ((match bb-source-stab)
              (pcase (string-to-number (match-string 1))
                ;; http://www.math.utah.edu/docs/info/stabs_11.html
                (68 (setq source-linum (string-to-number (match-string 2))))
                ((or 100 132) (setq source-linum nil))))
             ((match bb-endblock)
              (setq reachable-label nil)))))))

(cl-defun bb--rainbowize (src-buffer)
  (bb-clear-rainbow-overlays)
  (let* ((background-hsl
          (ignore-errors
            (apply #'color-rgb-to-hsl (color-name-to-rgb (face-background 'default)))))
         all-ovs
         (idx 0)
         total
         (shuffle (buffer-local-value 'bb-shuffle-rainbow src-buffer))
         (ht (make-hash-table)))
    (cl-loop initially (goto-char (point-min))
             with current-line = 1
             for (asm-region . src-line) in bb--line-mappings
             for (begl . endl) = asm-region
             do (push (cons (progn
                              (forward-line (- begl current-line))
                              (line-beginning-position))
                            (progn
                              (forward-line (- endl begl))
                              (setq current-line endl)
                              (line-end-position)))
                      (gethash src-line ht)))
    ;; The 1+ helps us keep our hue distance from the actual
    ;; background color
    (setq total (1+ (hash-table-count ht)))
    (unless background-hsl (cl-return-from bb--rainbowize nil))
    (maphash
     (lambda (src-line asm-pos-regions)
       (when (not (zerop src-line))
         (cl-loop
          with i = (if shuffle
                       (mod (* 27 (cl-incf idx)) total)
                     (cl-incf idx))
          with bright-hsl =(list (mod (+ (cl-first background-hsl)
                                         (/ i (float total)))
                                      1)
                                 (min (max (cl-second background-hsl)
                                           0.25)
                                      0.8)
                                 (min (max (cl-third background-hsl)
                                           0.25)
                                      0.8))
          with muted-hsl = (list (car bright-hsl)
                                 (/ (cadr bright-hsl) 2.0)
                                 (caddr bright-hsl))
          with color = (apply #'color-rgb-to-hex (apply #'color-hsl-to-rgb bright-hsl))
          with muted-color = (apply #'color-rgb-to-hex (apply #'color-hsl-to-rgb muted-hsl))
          for (beg . end) in asm-pos-regions
          for asm-ov = (make-overlay beg end)
          do
          (overlay-put asm-ov 'priority 0)
          (push asm-ov all-ovs)
          (overlay-put asm-ov 'face `(:background ,color))
          (overlay-put asm-ov 'beardbolt-rainbow-face `(:background ,color))
          (overlay-put asm-ov 'beardbolt-muted-face `(:background ,muted-color))
          (overlay-put asm-ov 'beardbolt 'asm)
          collect asm-ov into this-lines-asm-overlays
          finally
          (with-current-buffer src-buffer
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- src-line))
              (let* ((ov (make-overlay (line-beginning-position)
                                       (1+ (line-end-position))))
                     (group (cons ov this-lines-asm-overlays)))
                (overlay-put ov 'beardbolt-related-overlays group)
                (dolist (o group)
                  (overlay-put o 'beardbolt-related-overlays group))
                (overlay-put ov 'face `(:background ,color))
                (overlay-put ov 'beardbolt-rainbow-face `(:background ,color))
                (overlay-put ov 'beardbolt-muted-face `(:background ,muted-color))
                (overlay-put ov 'beardbolt t)
                (push ov all-ovs)))))))
     ht)
    (mapc #'delete-overlay bb--rainbow-overlays)
    (setq bb--rainbow-overlays all-ovs)))

(cl-defmacro bb--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(defun bb-clear-rainbow-overlays ()
  "Clear rainbow from the source file and asm output."
  (interactive)
  (let ((output-buffer (if (eq major-mode 'bb--asm-mode)
                           (current-buffer)
                         bb--asm-buffer)))
    (bb--when-live-buffer output-buffer
      (bb--when-live-buffer bb--source-buffer
        (save-restriction
          (widen)
          (cl-loop for o in (overlays-in (point-min) (point-max))
                   when (overlay-get o 'beardbolt) do (delete-overlay o)))))
      (mapc #'delete-overlay bb--rainbow-overlays)
      (setq bb--rainbow-overlays nil)))

;;;;; Handlers
(cl-defun bb--handle-finish-compile (compilation-buffer str)
  "Finish hook for compilations.  Runs in buffer COMPILATION-BUFFER.
Argument STR compilation finish status."
  (let* ((dump-file-name bb--dump-file)
         (src-buffer bb--source-buffer)
         (origin-window (or (get-buffer-window src-buffer)
                            (selected-window)))
         (compile-spec bb--compile-spec)
         (declared-output bb--declared-output)
         (asm-buffer (bb--asm-buffer src-buffer)))
    (delete-file dump-file-name)
    (with-current-buffer asm-buffer
      (bb--asm-mode)
      (setq bb--source-buffer src-buffer)
      (let* ((inhibit-modification-hooks t)
             (inhibit-read-only t)
             (window
              (with-selected-window origin-window
                (display-buffer asm-buffer '(nil (inhibit-same-window . t))))))
        (erase-buffer)
        (cond
         ((string-match "^finished" str)
          (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
          (insert-file-contents declared-output)
          (setq bb--line-mappings nil)
          (save-excursion (funcall (cadr compile-spec) dump-file-name))
          (setq bb--line-mappings (reverse bb--line-mappings))
          (when (bb--get bb-demangle)
            (shell-command-on-region (point-min) (point-max) "c++filt"
                                     (current-buffer) 'no-mark))
          (bb--rainbowize src-buffer))
         (t
          (insert "<Compilation failed>")))
        (unless (or (string-match "^interrupt" str)
                    (get-buffer-window compilation-buffer)
                    (and (string-match "^finished" str)
                         (not (bb--get bb-execute))))
          (with-selected-window window
            (let ((cwindow
                   (display-buffer compilation-buffer
                                   `((display-buffer-below-selected)))))
              (set-window-dedicated-p cwindow 'bb-dedication))))))))

(defun bb--compilation-buffer (&rest _)
  (get-buffer-create "*bb-compilation*"))

;;;;; UI Functions
(defun bb-compile (lang-desc)
  "Run beardbolt on current buffer for LANG-DESC.
LANG-DESC is an element of `beardbolt-languages'.  Interactively,
determine LANG from `major-mode'."
  (interactive (list (assoc major-mode bb-languages)))
  (bb--maybe-stop-running-compilation)
  (mapatoms (lambda (s) (when (get s 'bb--option) (kill-local-variable s))))
  (cl-letf (((symbol-function 'hack-local-variables-confirm)
             (lambda (_all-vars unsafe-vars risky-vars &rest _)
               (when unsafe-vars
                 (message "[beardbolt] Some variables unsafe %s" unsafe-vars))
               (when risky-vars
                 (message "[beardbolt] Some variables risky %s" risky-vars)))))
    (hack-local-variables))
  (let* ((dump-file (make-temp-file "beardbolt-dump-" nil
                                    (concat "." (file-name-extension buffer-file-name))))
         (src-buffer (current-buffer))
         (specs (apply (cadr lang-desc) (cddr lang-desc)))
         (spec (alist-get
                (if bb-disassemble :compile-assemble-disassemble :compile)
                specs))
         (command-and-declared-output (funcall (car spec) dump-file))
         (cmd (car command-and-declared-output))
         (cmd (mapconcat
               (lambda (s) (if (stringp s) s
                             (mapconcat #'identity (flatten-list s) " ")))
               (ensure-list cmd) " \\\n")))
    (let ((inhibit-message t))
      (write-region (point-min) (point-max) dump-file))
    (with-current-buffer ; With compilation buffer
        (let ((shell-file-name (or (executable-find bb--shell)
                                   shell-file-name))
              (compilation-auto-jump-to-first-error t))
          ;; TODO should this be configurable?
          (compilation-start cmd nil #'bb--compilation-buffer))
      ;; Only jump to errors, skip over warnings
      (setq-local compilation-skip-threshold 2)
      (setq-local compilation-always-kill t)
      (setq-local inhibit-message t)
      (add-hook 'compilation-finish-functions #'bb--handle-finish-compile nil t)
      (setq bb--source-buffer src-buffer)
      (setq bb--compile-spec spec)
      (setq bb--dump-file dump-file)
      (setq bb--declared-output (cdr command-and-declared-output)))))

(defun bb--maybe-stop-running-compilation ()
  (let ((buffer (bb--compilation-buffer)))
    (when-let ((proc (get-buffer-process buffer)))
      (set-process-query-on-exit-flag proc nil)
      (interrupt-process proc))))

;;;; Keymap
(defvar bb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'bb-compile)
    (define-key map (kbd "C-c C-d") #'bb-clear-rainbow-overlays)
    map)
  "Keymap for function `bb-mode'.")

;;;;; Starter Definitions

(defvar bb-starter-files
  '(("c++" . "beardbolt.cpp")
    ("c" . "beardbolt.c")
    ("rust" . "beardbolt.rs")))

;;;###autoload
(defun bb-starter (lang-name)
  "Setup new sandbox file for experiments.
With prefix argument, choose from starter files in `bb-starter-files'."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Language: " bb-starter-files nil t)
           (caar bb-starter-files))))
  (let* ((starter-file-name (cdr (assoc lang-name bb-starter-files)))
         (base (file-name-base starter-file-name))
         (ext (file-name-extension starter-file-name))
         (sandbox-file
          (expand-file-name (concat base "-"
                                    (format-time-string "%FT%T%z")
                                    "." ext)
                            (bb--sandbox-dir)))
         (src-file-name
          (when bb-dir
            (expand-file-name starter-file-name
                              (expand-file-name "starters/" bb-dir))))
         (src-file-exists (when src-file-name
                            (file-exists-p src-file-name))))
    (if (not src-file-exists)
        (error "Could not find starter files!")
      (unless (file-exists-p sandbox-file)
        (copy-file src-file-name sandbox-file))
      (find-file sandbox-file)
      (bb-mode 1))))

(defun bb--recenter-maybe (ov)
  (bb--when-live-buffer (overlay-buffer ov)
    (cl-loop with pos = (overlay-start ov)
             for w in (cl-remove-if (lambda (w)
                                      (and (>= pos (* 1.1 (window-start w)))
                                           (<= pos (* 0.9 (window-end w)))))
                                    (get-buffer-window-list))
             unless (eq w (selected-window))
             do (set-window-point w pos)
             (with-selected-window w (recenter)))))

(defvar bb--currently-synched-overlays nil)

(defun bb--synch-relation-overlays ()
  (let* ((at-point (overlays-at (point)))
         (all-ovs (if (eq major-mode 'bb--asm-mode)
                      bb--rainbow-overlays
                    (and bb--asm-buffer
                         (buffer-local-value 'bb--rainbow-overlays bb--asm-buffer))))
         (ov (cl-find-if (lambda (ov) (overlay-get ov 'beardbolt-rainbow-face))
                         at-point)))
    (cond ((and ov (not (member ov bb--currently-synched-overlays)))
           (dolist (o all-ovs)
             (overlay-put o 'face (overlay-get o 'beardbolt-muted-face)))
           (setq bb--currently-synched-overlays
                 (overlay-get ov 'beardbolt-related-overlays))
           (setq bb--currently-synched-overlays
                 (cl-sort bb--currently-synched-overlays #'< :key #'overlay-start))
           (dolist (o bb--currently-synched-overlays)
             (overlay-put o 'face 'bb-current-line-face))
           (let* ((other-buffer-overlays
                   (cl-remove (current-buffer)
                              bb--currently-synched-overlays
                              :key #'overlay-buffer))
                  (recenter-target (car other-buffer-overlays))
                  (pulse-delay 0.01)
                  (asm-overlays
                   (cl-remove-if-not (lambda (ov)
                                       (eq 'asm (overlay-get ov 'beardbolt)))
                                     bb--currently-synched-overlays)))
             (if (memq recenter-target asm-overlays)
                 (message "[beardbolt] maps to %s asm regions."
                          (length asm-overlays))
               (message "[beardbolt] asm region %s/%s for source line %s."
                        (1+ (cl-position ov asm-overlays))
                        (length asm-overlays)
                        (with-current-buffer (overlay-buffer recenter-target)
                          (line-number-at-pos (overlay-start recenter-target)))))
             (bb--recenter-maybe recenter-target)
             (pulse-momentary-highlight-overlay recenter-target
                                                'bb-current-line-face)))
          ((not ov)
           (dolist (o all-ovs)
             (overlay-put o 'face (overlay-get o 'beardbolt-rainbow-face)))
           (setq bb--currently-synched-overlays nil)))))

(defvar bb--change-timer nil)

(defun bb--after-change (&rest _)
  (bb-clear-rainbow-overlays)
  (when bb-compile-delay
    (when (timerp bb--change-timer) (cancel-timer bb--change-timer))
    (setq bb--change-timer
          (run-with-timer bb-compile-delay nil #'bb-compile
                          (assoc major-mode bb-languages)))))

(defun bb--guess-from-ccj ()
  (if-let* ((ccj-basename "compile_commands.json")
            (ccj-dir (locate-dominating-file default-directory ccj-basename))
            (ccj-file (expand-file-name ccj-basename ccj-dir))
            (ccj (with-temp-buffer
                   (insert-file-contents ccj-file)
                   (goto-char (point-min))
                   (json-parse-buffer :object-type 'plist)))
            (cmd (cl-loop for e across ccj
                          for file = (plist-get e :file)
                          when (equal file buffer-file-name)
                          return (plist-get e :command)))
            (cmd (bb--split-rm-double cmd "-o"))
            (cmd (bb--split-rm-double cmd "-c"))
            (cmd (bb--split-rm-single cmd "-flto" #'string-prefix-p)))
      (list cmd bb-ccj-extra-flags)))

;;;###autoload
(define-minor-mode bb-mode
  "Toggle `beardbolt-mode'.  May be enabled by user in source buffer."
  :global nil :lighter " ⚡" :keymap bb-mode-map
  (cond
   (bb-mode
    (add-hook 'after-change-functions #'bb--after-change nil t)
    (add-hook 'post-command-hook #'bb--synch-relation-overlays nil t))
   (t
    (remove-hook 'after-change-functions #'bb--after-change t)
    (remove-hook 'post-command-hook #'bb--synch-relation-overlays t))))

(define-derived-mode bb--asm-mode asm-mode "⚡asm ⚡"
  "Toggle `bearbolt--output-mode', internal mode for asm buffers."
  (add-hook 'kill-buffer-hook #'bb-clear-rainbow-overlays nil t)
  (add-hook 'post-command-hook #'bb--synch-relation-overlays nil t)
  (setq truncate-lines t)
  (read-only-mode t)
  (buffer-disable-undo)
  (local-set-key (kbd "q") 'quit-window))

(provide 'beardbolt)

;;; beardbolt.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("bb-" . "beardbolt-"))
;; End:
