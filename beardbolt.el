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
;;;; Customize:
(defgroup beardbolt nil
  "beardbolt customization options"
  :group 'applications)

(defcustom bb-disassemble nil
  "Whether we should disassemble an output binary."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)

(defcustom bb-command nil
  "The base command to run beardbolt from."
  :type 'string
  ;; nil means use default command
  :safe (lambda (v) (or (booleanp v) (listp v) (stringp v)))
  :group 'beardbolt)

(defcustom bb-asm-format 'att
  "Which output assembly format to use.

The supported values depend highly on the exporter, but typical
values are: `intel', `att' or nil (for using tool defaults).
Invalid values will be passed onto the disassembly tools, which
may throw errors.

If you are not on x86, you most likely want to set this to nil."
  :type 'string
  :safe (lambda (v) (or (booleanp v) (symbolp v) (stringp v)))
  :group 'beardbolt)
(defcustom bb-preserve-directives nil
  "Whether to preserve assembly directives."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(defcustom bb-preserve-labels nil
  "Whether to preserve unused labels."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(defcustom bb-preserve-weak-symbols t
  "Whether to preserve library function."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(defcustom bb-preserve-comments nil
  "Whether to filter comment-only lines."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(defcustom bb-ignore-binary-limit nil
  "Whether to ignore the binary limit. Could hang emacs..."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(defcustom bb-demangle t
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
(defvar-local bb--line-mappings (make-hash-table) "Maps source lines -> asm regions")
(defvar-local bb--relation-overlays nil "Overlays relating source to asm.")
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

(defvar bb-defines-weak (rx bol (0+ space) ".weak"
                            (0+ space)
                            (group (any ".a-zA-Z_")
                                   (0+ (any "a-zA-Z0-9$_.")))))
(defvar bb-label-reference (rx (any ".a-zA-Z_")
                               (0+
                                (any "a-zA-Z0-9$_."))))
(defvar bb-set-directive (rx bol (0+ space) ".set" (1+ space)
                             (group (any ".a-zA-Z_")
                                    (0+ (any "a-zA-Z0-9$_.")))
                             (0+ space) "," (0+ space)
                             (group (any ".a-zA-Z_")
                                    (0+ (any "a-zA-Z0-9$_.")))))
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
           (debug `("-g"))
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

(defvar bb--demangle-cache (make-hash-table :test #'equal))

(cl-defun bb--demangle-quick (from to)
  (let* ((s (buffer-substring-no-properties from to))
         (probe (gethash s bb--demangle-cache)))
    (when probe
      (delete-region from to)
      (goto-char from)
      (insert probe)
      t)))

(cl-defun bb--demangle-overlays (ovs)
  (cl-loop
   with rep = (lambda (ov r)
                (with-current-buffer (overlay-buffer ov)
                  (delete-region (overlay-start ov) (overlay-end ov))
                  (goto-char (overlay-start ov))
                  (insert r)
                  (delete-overlay ov)))
   for ov in ovs
   for from = (overlay-start ov) for to = (overlay-end ov)
   for s = (buffer-substring-no-properties from to)
   for probe = (gethash s bb--demangle-cache)
   if probe do (funcall rep ov probe)
   else collect ov into needy-overlays
   and collect s into needy-strings
   and concat (format "%s\n" s) into tosend
   finally
   (when needy-strings
     (with-temp-buffer
       (save-excursion (insert tosend))
       (shell-command-on-region (point-min) (point-max) "c++filt" t t)
       (cl-loop for ov in needy-overlays for s in needy-strings
                while (re-search-forward "^.*$")
                do (funcall rep ov (puthash s (match-string 0) bb--demangle-cache)))))))

(defun bb--user-func-p (func)
  "Tell if FUNC is user's."
  (let* ((regexp bb--hidden-func-c))
    (if regexp (not (string-match-p regexp func)) t)))

(defmacro bb--sweeping (&rest forms)
  (declare (indent 0)
           (debug (&rest (form &rest form))))
  (let ((lbp (cl-gensym "lbp-")) (lep (cl-gensym "lep-"))
        (preserve-directives (cl-gensym "preserve-directives-")))
    `(let ((,preserve-directives (buffer-local-value
                                  'bb-preserve-directives
                                  bb--source-buffer)))
       (goto-char (point-min))
       (while (not (eobp))
         (let ((,lbp (line-beginning-position)) (,lep (line-end-position)))
           (cl-macrolet ((match (&rest res)
                           `(cl-loop for re in ,(cons 'list res)
                                     thereis (re-search-forward re ,',lep t)))
                         (update-lep () `(setq ,',lep (line-end-position))))
             (pcase (cond ,@forms)
               (:preserve (forward-line 1))
               (:kill (delete-region ,lbp (1+ ,lep)))
               (_
                (if ,preserve-directives (forward-line 1)
                  (delete-region ,lbp (1+ ,lep)))))))))))

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
       (when nil
         (add-text-properties (line-beginning-position) (line-end-position)
                              `(bb-src-line ,source-linum)))
       (replace-match (concat (match-string 1) "\t" (match-string 3)))
       (forward-line 1))
      (t
       :kill))))

(cl-defun bb--reachable-p (label globals graph synonyms weaks)
  (let ((synonym (gethash label synonyms)))
    (cond ((and weaks (intern-soft label weaks))
           nil)
          ((intern-soft label globals) t)
          (t
           (maphash (lambda (from to)
                      (when (and (or (intern-soft label to)
                                     (and synonym (intern-soft synonym to)))
                                 (bb--reachable-p from globals graph synonyms weaks))
                        (cl-return-from bb--reachable-p
                          (progn
                            (when synonym (intern synonym globals))
                            (intern label globals)))))
                    graph)))))

(defun bb--process-asm ()
  (let ((globals (obarray-make))
        (weaks (obarray-make))
        (synonyms (make-hash-table :test #'equal))
        (label-graph (make-hash-table :test #'equal))
        (src-file-name "<stdin>")
        (source-file-map (make-hash-table :test #'eq))
        (source-linum nil)
        global-label
        reachable-label
        demangle-ovs
        (preserve-comments (buffer-local-value 'bb-preserve-comments bb--source-buffer))
        (preserve-labels (buffer-local-value 'bb-preserve-labels bb--source-buffer))
        (preserve-weak-symbols (buffer-local-value 'bb-preserve-weak-symbols bb--source-buffer)))
    (cl-flet ((schedule-demangling-maybe (from to)
                (when (and (eq (char-after from) ?_)
                           (not (bb--demangle-quick from to)))
                  (let ((ov (make-overlay from to)))
                    (overlay-put ov 'beardbolt t)
                    (push ov demangle-ovs)))))
      ;; first pass
      (bb--sweeping
        ((match bb-data-defn) :preserve)
        ((match bb-label-start)
         (when (intern-soft (match-string 1) globals)
           (setq global-label (match-string 1)))
         :preserve)
        ((match bb-source-tag)
         (setq source-linum
               (and (equal src-file-name
                           (gethash
                            (string-to-number (match-string 1))
                            source-file-map))
                    (string-to-number (match-string 2)))))
        ((match bb-has-opcode)
         (when source-linum
           (add-text-properties
            (match-beginning 0) (match-end 0)
            (list 'bb-src-line source-linum)))
         (when global-label
           (while (match bb-label-reference)
             (intern (match-string 0)
                     (or (gethash global-label label-graph)
                         (puthash global-label (obarray-make)
                                  label-graph)))
             (schedule-demangling-maybe (match-beginning 0) (match-end 0))
             (update-lep)))
         :preserve)
        ((and (not preserve-comments) (match bb-comment-only)) :kill)
        ((match bb-defines-global bb-defines-function-or-object)
         (intern (match-string 1) globals))
        ((and (not preserve-weak-symbols) (match bb-defines-weak))
         (intern (match-string 1) weaks))
        ((match bb-source-file-hint)
         (puthash (string-to-number (match-string 1))
                  (or (match-string 3) (match-string 2))
                  source-file-map))
        ((match bb-endblock) (setq global-label nil) :preserve)
        ((match bb-set-directive)
         (puthash (match-string 2) (match-string 1) synonyms))
        ((match bb-source-stab)
         (pcase (string-to-number (match-string 1))
           ;; http://www.math.utah.edu/docs/info/stabs_11.html
           (68 (setq source-linum (match-string 2)))
           ((or 100 132) (setq source-linum nil)))))
      ;; second pass
      (setq reachable-label nil)
      (bb--sweeping
        ((and (match bb-data-defn bb-has-opcode) reachable-label)
         :preserve)
        ((match bb-label-start)
         (cond
          ((bb--reachable-p (match-string 1) globals label-graph synonyms
                            (unless preserve-weak-symbols weaks))
           (setq reachable-label (match-string 1))
           (schedule-demangling-maybe (match-beginning 0) (match-end 0))
           :preserve)
          (t
           (if preserve-labels :preserve :kill))))
        ((match bb-endblock) (setq reachable-label nil)))
      (bb--demangle-overlays demangle-ovs))))

(cl-defun bb--rainbowize (src-buffer)
  (let* ((background-hsl
          (ignore-errors
            (apply #'color-rgb-to-hsl (color-name-to-rgb (face-background 'default)))))
         all-ovs
         (idx 0)
         ;; The 1+ helps us keep our hue distance from the actual
         ;; background color
         (total (1+ (hash-table-count bb--line-mappings))))
    (unless background-hsl (cl-return-from bb--rainbowize nil))
    (maphash
     (lambda (src-line asm-regions)
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
          for (beg . end) in (cl-getf asm-regions :positions)
          for asm-ov = (make-overlay beg end)
          do
          (overlay-put asm-ov 'priority 0)
          (push asm-ov all-ovs)
          (overlay-put asm-ov 'face `(:background ,color))
          (overlay-put asm-ov 'beardbolt t)
          finally
          (with-current-buffer src-buffer
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- src-line))
              (let ((ov (make-overlay (line-beginning-position)
                                      (1+ (line-end-position)))))
                (push ov all-ovs)
                (overlay-put ov 'face `(:background ,color))
                (overlay-put ov 'beardbolt t)
                (overlay-put ov 'priority 0)))))))
     bb--line-mappings)
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

(defun bb--make-line-mappings ()
  (let ((linum 1)
        (start-match nil)
        (in-match nil)
        (ht bb--line-mappings))
    (clrhash ht)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((property (get-text-property (point) 'bb-src-line)))
          (progn
            (cl-tagbody
             run-conditional
             (cond
              ((and in-match (eq in-match property))
               ;; We are continuing an existing match
               nil)
              (in-match
               ;; We are in a match that has just expired
               (push (cons start-match (1- linum))
                     (cl-getf (gethash in-match ht) :lines))
               (setq in-match nil
                     start-match nil)
               (go run-conditional))
              (property
               (setq in-match property
                     start-match linum))))))
        (cl-incf linum)
        (forward-line 1)))
    (maphash (lambda (_k asm-regions)
               (save-excursion
                 (plist-put
                  asm-regions
                  :positions
                  (cl-loop
                   for (begl . endl) in (cl-getf asm-regions :lines)
                   collect (cons (progn
                                   (goto-char (point-min))
                                   (forward-line (1- begl))
                                   (line-beginning-position))
                                 (progn
                                   (forward-line (- endl begl))
                                   (line-end-position)))))))
             ht)
    ht))

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
          (cond ((eq
                  t (while-no-input
                      (save-excursion (funcall (cadr compile-spec)))))
                 (erase-buffer)
                 (insert "Interrupted!"))
                (t
                 (when output-window
                   (set-window-start output-window old-window-start)
                   (set-window-point output-window old-point))
                 (bb--make-line-mappings)
                 (bb--rainbowize src-buffer))))
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

;;;; Overlay Commands
(defun bb--make-relation-overlay (start end)
  "Setup overlay with START and END in BUF."
  (let ((o (make-overlay start end)))
    (overlay-put o 'face 'bb-current-line-face)
    (overlay-put o 'priority 1)
    (overlay-put o 'beardbolt t)
    (overlay-put o 'beardbolt-relation t)
    o))

(defun bb--recenter-maybe (pos)
  (cl-loop for w in (cl-remove-if (lambda (w)
                                    (and (>= pos (* 1.1 (window-start w)))
                                         (<= pos (* 0.9 (window-end w)))))
                                  (get-buffer-window-list))
           unless (eq w (selected-window))
           do (set-window-point w pos)
           (with-selected-window w (recenter))))

(defun bb--synch-relation-overlays (source-line)
  "Update overlays to visually match selected source and asm lines.
Runs in output buffer.  Sets `bb--relation-overlays'."
  (bb--delete-relation-overlays)
  (let* ((positions (plist-get (gethash source-line bb--line-mappings)
                               :positions))
         (src-overlay
          (and positions
               (bb--when-live-buffer bb--source-buffer
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- source-line))
                   (bb--recenter-maybe (point))
                   (bb--make-relation-overlay
                    (line-beginning-position)
                    (line-end-position)))))))
    (when src-overlay
      (push src-overlay bb--relation-overlays)
      (cl-loop for (start . end) in positions
               do (push (bb--make-relation-overlay start end) bb--relation-overlays)
               finally (bb--recenter-maybe (caar positions))))))

(defun bb--delete-relation-overlays ()
  (mapc #'delete-overlay bb--relation-overlays)
  (setq bb--relation-overlays nil))

(defun bb--source-buffer-pch ()
  (let ((linum (line-number-at-pos nil t)))
    (bb--when-live-buffer bb--output-buffer
      (bb--synch-relation-overlays linum))))

(defun bb--on-kill-source-buffer ()
  (bb--when-live-buffer bb--output-buffer
    (kill-buffer bb--output-buffer)))

(defun bb--on-kill-output-buffer ()
  (bb--delete-relation-overlays)
  (bb--delete-rainbow-overlays))

(defun bb--output-buffer-pch ()
  (bb--synch-relation-overlays (get-text-property (point) 'bb-src-line)))

(defvar bb--change-timer nil)

(defun bb--after-change (&rest _)
  (bb--when-live-buffer bb--output-buffer
    (when bb--line-mappings (clrhash bb--line-mappings)))
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
