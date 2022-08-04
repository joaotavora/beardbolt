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

;;; Code:
(defgroup beardbolt nil
  "beardbolt customization options"
  :group 'applications)

(defmacro bb--defoption (sym &rest whatever)
  `(progn (defcustom ,sym ,@whatever)
          (put ',sym 'bb--option t)))


(bb--defoption bb-disassemble nil
  "Whether we should disassemble an output binary."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)

(bb--defoption bb-kill-symbol-re nil
  "Regular expression matching assembly symbols to ignore.
Currently, this matches on **mangled** symbols.

A somewhat useful value could be

   \\(^_Z[^0-9]*[SP]\\|__gnu\\)

in quotes, of course."
  :type 'string
  :safe (lambda (v) (or (booleanp v) (stringp v)))
  :group 'beardbolt)

(bb--defoption bb-command nil
  "The base command to run beardbolt from."
  :type 'string
  ;; nil means use default command
  :safe (lambda (v) (or (booleanp v) (listp v) (stringp v)))
  :group 'beardbolt)

(bb--defoption bb-asm-format 'att
  "Which output assembly format to use.
Passed directly to compiler or disassembler."
  :type 'string
  :safe (lambda (v) (or (booleanp v) (symbolp v) (stringp v)))
  :group 'beardbolt)
(bb--defoption bb-preserve-directives nil
  "Whether to preserve assembly directives."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(bb--defoption bb-preserve-labels nil
  "Whether to preserve unused labels."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(bb--defoption bb-preserve-comments nil
  "Whether to filter comment-only lines."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(bb--defoption bb-demangle t
  "Whether to attempt to demangle the resulting assembly."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)

;;;; Faces

(defface bb-current-line-face
  '((t (:weight bold :inherit highlight)))
  "Face to fontify the current line for showing matches."
  :group 'beardbolt)

;;;; Basic model
(defvar-local bb--output-buffer nil)
(defvar-local bb--source-buffer nil)
(defvar-local bb--compile-spec nil)
(defvar-local bb--declared-output nil)
(defvar-local bb--dump-file nil "Temporary file")
(defvar-local bb--line-mappings nil "Maps asm regions -> source lines")
(defvar-local bb--rainbow-overlays nil "Rainbow overlays.")

(defun bb--output-buffer (src-buffer)
  "Get/create output buffer for current source file."
  (with-current-buffer src-buffer
    (or (and (buffer-live-p bb--output-buffer) bb--output-buffer)
        (setq bb--output-buffer
              (with-current-buffer
                  (generate-new-buffer (format "*bb-output for %s*" src-buffer))
                (current-buffer))))))

(defvar bb-hide-compile t)

(defvar bb-compile-delay 1.0
  "Time in seconds to delay before recompiling if there is a change.")

(defvar bb--shell "bash"
  "Which shell to prefer if available.
Used to work around inconsistencies in alternative shells.")

(defvar bb--temp-dir nil
  "Temporary directory to use for compilation and other reasons.")

(defun bb--temp-dir ()
  (or (and bb--temp-dir (file-exists-p bb--temp-dir) bb--temp-dir)
      (setq bb--temp-dir (make-temp-file "beardbolt-bb-" t)))) 

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
(defvar bb-label-reference (rx (any ".a-zA-Z_")
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

(cl-defstruct (bb-lang
               (:constructor make-beardbolt-lang)
               (:conc-name bb--lang-))
  (base-cmd nil :documentation "") (compile-specs nil :documentation ""))

(defun bb-split-rm-single (cmd flag &optional test)
  "Remove a single FLAG from CMD.  Test according to TEST."
  (mapconcat #'identity (cl-remove flag (split-string cmd)
                                   :test (or test #'string=))
             " "))

(defun bb-split-rm-double (cmd flag)
  "Remove a single FLAG and arg from CMD."
  (cl-loop while split with split = (split-string cmd)
           for i from 0
           for probe = (car split)
           if (string= probe flag) do (setq split (cddr split))
           else
           concat (and (cl-plusp i) " ")
           and concat probe and do (setq split (cdr split))))

(cl-defun bb--c/c++-compile-specs ()
  "Process a compile command for gcc/clang.
Returns a list (SPEC ...) where SPEC looks like (WHAT FN CMD)."
  (cl-labels ((tmp (f newext)
                (expand-file-name
                 (format "%s.%s" (file-name-base f) newext) (bb--temp-dir)))
              (objdump (in)
                (let ((out (tmp in "bb-objdumped")))
                  `(("&&" ,bb-objdump-binary "-d" ,in
                     "--insn-width=16" "-l"
                     ,(when bb-asm-format
                        (format "-M %s" bb-asm-format))
                     ">" ,out)
                    . ,out)))
              (join (l &optional (sep " ")) (mapconcat #'identity l sep))
              (munch (l) (join (mapcar #'join l) " \\\n")))
    (let* ((direct-asm-out (tmp "beardbolt" "s"))
           (disass-asm-out (tmp "beardbolt" "out"))
           (base-command (ensure-list (or bb-command
                                          (bb--lang-base-cmd (bb--get-lang)))))
           (debug `("-g1"))
           (stdin-process `("-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-"))
           (direct-asm `("-S" ,(format "-masm=%s" bb-asm-format)
                         "-o" ,direct-asm-out))
           (disass-asm `("-c" "-o" ,disass-asm-out)))
      `((:compile
         ,(lambda (dump-file)
            (cons
             (munch `(,base-command ,stdin-process ,debug
                                    ,direct-asm ("<" ,dump-file)))
             direct-asm-out))
         ,#'bb--process-asm)
        (:compile-assemble-disassemble
         ,(lambda (dump-file)
            (let* ((objdump-pair (objdump disass-asm-out)))
              (cons
               (munch `(,base-command ,stdin-process ,debug
                                      ,disass-asm ("<" ,dump-file)
                                      ,(car objdump-pair)))
               (cdr objdump-pair))))
         ,#'bb--process-disassembled-lines)))))

(defvar bb--hidden-func-c
  (rx bol (or (and "__" (0+ any))
              (and "_" (or "init" "start" "fini"))
              (and (opt "de") "register_tm_clones")
              "call_gmon_start"
              "frame_dummy"
              (and ".plt" (0+ any)))
      eol))

(defvar bb-languages
  `((c-mode
     . ,(make-beardbolt-lang :compile-specs #'bb--c/c++-compile-specs
                             :base-cmd "gcc"))
    (c++-mode
     . ,(make-beardbolt-lang :compile-specs #'bb--c/c++-compile-specs
                             :base-cmd "g++"))))

(defmacro bb-with-display-buffer-no-window (&rest body)
  "Run BODY without displaying any window."
  ;; See http://debbugs.gnu.org/13594
  `(let ((display-buffer-overriding-action (list #'display-buffer-no-window)))
     ,@body))

(defun bb--user-func-p (func)
  "Tell if FUNC is user's."
  (let* ((regexp bb--hidden-func-c))
    (if regexp (not (string-match-p regexp func)) t)))

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

(cl-defun bb--process-disassembled-lines ()
  (let* ((src-file-name "<stdin>") (func nil) (source-linum nil))
    (bb--sweeping
      ((match bb-disass-line)
       (setq source-linum (and (equal src-file-name
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
       :preserve))))

(defun bb--process-asm ()
  (let* ((used-labels (obarray-make))
         (maybe-mark-used (lambda (s)
                            (unless (and (bb--get bb-kill-symbol-re)
                                         (string-match
                                          (bb--get bb-kill-symbol-re)
                                          s))
                              (intern s used-labels))))
         (src-file-name "<stdin>")
         (source-file-map (make-hash-table :test #'eq))
         source-linum
         global-label
         reachable-label
         (preserve-comments (bb--get bb-preserve-comments))
         (preserve-labels (bb--get bb-preserve-labels)))
    (bb--sweeping ; first pass
      ((not (eq (char-after) ?\t))
       (cond ((match bb-label-start)
              (when (intern-soft (match-string 1) used-labels)
                (setq global-label (match-string 1)))
              :preserve)
             (t :kill)))
      (t
       (cond ((match bb-has-opcode)
              (when global-label
                (while (match bb-label-reference)
                  (funcall maybe-mark-used (match-string 0))))
              :preserve)
             ((and (not preserve-comments) (match bb-comment-only)) :kill)
             ((match bb-defines-global bb-defines-function-or-object)
              (funcall maybe-mark-used (match-string 1)))
             ((match bb-source-file-hint)
              (puthash (string-to-number (match-string 1))
                       (or (match-string 3) (match-string 2))
                       source-file-map))
             ((match bb-endblock) (setq global-label nil)
              :preserve)
             (t :preserve))))
    (bb--sweeping ; second pass
      ((not (eq (char-after) ?\t))
       (when (match bb-label-start)
         (cond
          ((intern-soft (match-string 1) used-labels)
           (setq reachable-label (match-string 1))
           :preserve)
          (t
           (if preserve-labels :preserve :kill)))))
      (t
       (cond ((and (match bb-data-defn) reachable-label)
              :preserve)
             ((and (match bb-has-opcode) reachable-label)
              (when source-linum (bb--register-mapping source-linum (asm-linum)))
              :preserve)
             ((match bb-source-tag)
              (setq source-linum
                    (and (equal src-file-name
                                (gethash
                                 (string-to-number (match-string 1))
                                 source-file-map))
                         (string-to-number (match-string 2)))))
             ((match bb-source-stab)
              (pcase (string-to-number (match-string 1))
                ;; http://www.math.utah.edu/docs/info/stabs_11.html
                (68 (setq source-linum (match-string 2)))
                ((or 100 132) (setq source-linum nil))))
             ((match bb-endblock)
              (setq reachable-label nil)))))))

(cl-defun bb--rainbowize (src-buffer)
  (bb--delete-rainbow-overlays)
  (let* ((background-hsl
          (ignore-errors
            (apply #'color-rgb-to-hsl (color-name-to-rgb (face-background 'default)))))
         all-ovs
         (idx 0)
         total
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
          with color =
          (apply #'color-rgb-to-hex
                 (color-hsl-to-rgb (mod (+ (cl-first background-hsl)
                                           (/ (cl-incf idx) (float total)))
                                        1)
                                   (min (max (cl-second background-hsl)
                                             0.25)
                                        0.8)
                                   (min (max (cl-third background-hsl)
                                             0.25)
                                        0.8)))
          for (beg . end) in asm-pos-regions
          for asm-ov = (make-overlay beg end)
          do
          (overlay-put asm-ov 'priority 0)
          (push asm-ov all-ovs)
          (overlay-put asm-ov 'face `(:background ,color))
          (overlay-put asm-ov 'beardbolt-rainbow-face `(:background ,color))
          (overlay-put asm-ov 'beardbolt t)
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
                (overlay-put ov 'beardbolt t)
                (push ov all-ovs)))))))
     ht)
    (mapc #'delete-overlay bb--rainbow-overlays)
    (setq bb--rainbow-overlays all-ovs)))

(cl-defmacro bb--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(defun bb--delete-rainbow-overlays ()
  (bb--when-live-buffer bb--source-buffer
    (save-restriction
      (widen)
      (cl-loop for o in (overlays-in (point-min) (point-max))
               when (overlay-get o 'beardbolt) do (delete-overlay o))))
  (mapc #'delete-overlay bb--rainbow-overlays)
  (setq bb--rainbow-overlays nil))

;;;;; Handlers
(cl-defun bb--handle-finish-compile (compilation-buffer str)
  "Finish hook for compilations.  Runs in buffer COMPILATION-BUFFER.
Argument STR compilation finish status."
  (delete-file bb--dump-file)
  (let* ((src-buffer bb--source-buffer)
         (compile-spec bb--compile-spec)
         (declared-output bb--declared-output)
         (output-buffer (bb--output-buffer src-buffer))
         (split-width-threshold (min split-width-threshold 100)))
    (with-current-buffer output-buffer
      (asm-mode)
      (display-line-numbers-mode)
      (setq bb--source-buffer src-buffer)
      (bb--output-mode)
      (buffer-disable-undo)
      ;; Store src buffer value for later linking
      (cond
       ((string-match "^finished" str)
        (display-buffer (current-buffer) `(() (inhibit-same-window . t)))
        ;; Replace buffer contents but save point and scroll
        (let* ((output-window (get-buffer-window))
               (inhibit-modification-hooks t)
               (old-point (and output-window (window-point output-window)))
               (old-window-start (and output-window (window-start output-window))))
          (erase-buffer)
          (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
          (insert-file-contents declared-output)
          (setq bb--line-mappings nil)
          (save-excursion (funcall (cadr compile-spec)))
          (when output-window
            (set-window-start output-window old-window-start)
            (set-window-point output-window old-point))
          (setq bb--line-mappings (reverse bb--line-mappings))
          (when (bb--get bb-demangle)
            (shell-command-on-region (point-min) (point-max) "c++filt"
                                     (current-buffer) 'no-mark))
          (bb--rainbowize src-buffer))
        (when-let ((w (get-buffer-window compilation-buffer)))
          (quit-window nil w)))
       (t
        (when-let ((w (get-buffer-window)))
          (quit-window t w))
        (unless (string-match "^interrupt" str)
          (display-buffer compilation-buffer '(nil (inhibit-same-window . t)))))))))

;;;;; Parsing Options
(defvar-local bb--language-descriptor nil)
(defun bb--get-lang ()
  "Helper function to get lang def for LANGUAGE."
  (or bb--language-descriptor
      (cdr (assoc major-mode bb-languages))))

(defun bb--compilation-buffer (&rest _)
  (get-buffer-create "*bb-compilation*"))

;;;;; UI Functions
(defun bb-compile (lang)
  "Run beardbolt on current buffer for LANG.
Interactively, determine LANG from `major-mode'."
  (interactive (list (bb--get-lang)))
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
         (specs (funcall (bb--lang-compile-specs lang)))
         (spec (alist-get
                (if bb-disassemble :compile-assemble-disassemble :compile)
                specs))
         (command-and-declared-output (funcall (car spec) dump-file))
         (cmd (car command-and-declared-output)))
    (let ((inhibit-message t))
      (write-region (point-min) (point-max) dump-file))
    (with-current-buffer ; With compilation buffer
        (let ((shell-file-name (or (executable-find bb--shell)
                                   shell-file-name))
              (compilation-auto-jump-to-first-error t))
          ;; TODO should this be configurable?
          (bb-with-display-buffer-no-window
           (compilation-start cmd nil #'bb--compilation-buffer)))
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
    map)
  "Keymap for function `bb-mode'.")

;;;;; Starter Definitions

(defvar bb-starter-files
  '(("c" . "beardbolt.c")
    ("c++" . "beardbolt.cpp") ))

;;;###autoload
(defun bb-starter (lang-name)
  "Setup new file based on the sample STARTER-FILE-NAME."
  (interactive
   (list (completing-read "Language: " bb-starter-files nil t)))
  (let* ((starter-file-name (cdr (assoc lang-name bb-starter-files)))
         (file-name
          (expand-file-name starter-file-name (bb--temp-dir)))
         (exists (file-exists-p file-name))
         (src-file-name
          (when bb-dir
            (expand-file-name starter-file-name
                              (expand-file-name "starters/" bb-dir))))
         (src-file-exists (when src-file-name
                            (file-exists-p src-file-name))))
    (if (not src-file-exists)
        (error "Could not find starter files! Are you sure the starter/ folder is available? If you want to overide, set `bb-dir' to your install path")
      (unless exists
        (copy-file src-file-name file-name))
      (find-file file-name)
      (bb-mode 1))))

(defun bb--recenter-maybe (pos)
  (cl-loop for w in (cl-remove-if (lambda (w)
                                    (and (>= pos (* 1.1 (window-start w)))
                                         (<= pos (* 0.9 (window-end w)))))
                                  (get-buffer-window-list))
           unless (eq w (selected-window))
           do (set-window-point w pos)
           (with-selected-window w (recenter))))

(defvar bb--currently-synched-overlays nil)

(defun bb--synch-relation-overlays ()
  (let* ((at-point (overlays-at (point)))
         has-recentered
         (ov (cl-find-if (lambda (ov) (overlay-get ov 'beardbolt-rainbow-face))
                         at-point)))
    (cond ((and ov (not (member ov bb--currently-synched-overlays)))
           (dolist (oov bb--currently-synched-overlays)
             (overlay-put oov 'face (overlay-get ov 'beardbolt-rainbow-face)))
           (setq bb--currently-synched-overlays
                 (overlay-get ov 'beardbolt-related-overlays))
           (dolist (oov bb--currently-synched-overlays)
             (unless (or has-recentered
                         (eq (overlay-buffer oov) (overlay-buffer ov)))
               (bb--when-live-buffer (overlay-buffer oov)
                 (bb--recenter-maybe (overlay-start oov))
                 (setq has-recentered t)))
             (overlay-put oov 'face 'bb-current-line-face)))
          ((not ov)
           (dolist (ov bb--currently-synched-overlays)
             (overlay-put ov 'face (overlay-get ov 'beardbolt-rainbow-face)))
           (setq bb--currently-synched-overlays nil)))))

(defun bb--source-buffer-pch ()
  (bb--synch-relation-overlays))

(defun bb--on-kill-source-buffer ()
  (bb--when-live-buffer bb--output-buffer
    (kill-buffer bb--output-buffer)))

(defun bb--on-kill-output-buffer ()
  (bb--delete-rainbow-overlays))

(defun bb--output-buffer-pch ()
  (bb--synch-relation-overlays))

(defvar bb--change-timer nil)

(defun bb--after-change (&rest _)
  (when (timerp bb--change-timer) (cancel-timer bb--change-timer))
  (setq bb--change-timer (run-with-timer bb-compile-delay nil #'bb--on-change-timer)))

(defun bb--on-change-timer ()
  (bb-compile (bb--get-lang)))

;;;; Mode Definition:

;;;###autoload
(define-minor-mode bb-mode
  "Toggle `beardbolt-mode'.  May be enabled by user in source buffer."
  :global nil :lighter " ⚡" :keymap bb-mode-map
  (cond
   (bb-mode
    (setq-local bb--language-descriptor (bb--get-lang))
    (add-hook 'after-change-functions #'bb--after-change nil t)
    (add-hook 'kill-buffer-hook #'bb--on-kill-source-buffer nil t)
    (add-hook 'post-command-hook #'bb--source-buffer-pch nil t))
   (t
    (remove-hook 'after-change-functions #'bb--after-change t)
    (remove-hook 'kill-buffer-hook #'bb--on-kill-source-buffer t)
    (remove-hook 'post-command-hook #'bb--source-buffer-pch t))))

(define-minor-mode bb--output-mode
  "Toggle `bearbolt--output-mode', internal mode for asm buffers."
  :global nil :lighter " ⚡"
  (cond
   (bb--output-mode
    (add-hook 'kill-buffer-hook #'bb--on-kill-output-buffer nil t)
    (add-hook 'post-command-hook #'bb--output-buffer-pch nil t))
   (t
    (remove-hook 'kill-buffer-hook #'bb--on-kill-output-buffer t)
    (remove-hook 'post-command-hook #'bb--output-buffer-pch t))))

;;;###autoload
(defun beardbolt ()
  "Start beardbolt compilation, enable `bearbolt-mode'."
  (interactive)
  (unless bb-mode (bb-mode))
  (bb-compile (bb--get-lang)))

(provide 'beardbolt)

;;; beardbolt.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("bb-" . "beardbolt-"))
;; End:
