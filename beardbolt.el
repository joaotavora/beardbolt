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
;;
;; It currently supports: C/C++, OCaml, Haskell, Python, Java, Go, PHP, D,
;; Pony, Zig, Swift, Emacs Lisp, and (limited) Common Lisp.
;;
;; Adding support for more languages, if they have an easy manual compilation
;; path from source->assembly/bytecode with debug information, should be much
;; easier than in other alternatives.
;;
;; It's able to do this by:
;; 1. Compiling changes automatically, adding options which cause the compiler
;; to output assembly/bytecode with debug information (or by using objdump)
;; 2. Parse assembly/bytecode to create a map from it to the original source
;; 3. Strip out unneeded information from bytecode to only show useful code
;; 4. Provide an interface for highlighting the matched assembly/bytecode line
;; to the source and vice versa
;;
;; Tweakables:
;; beardbolt is primarily configured with Emacs local variables. This lets you
;; change compiler and beardbolt options simply by editing a local variable block.
;;
;; Notable options:
;; `beardbolt-command': prefix of the compilation command to use.
;; `beardbolt-default-directory': default-drectory to compile from.
;; `beardbolt-disassemble': disassemble from compiled binary with objdump
;; `beardbolt-filter-*': Tweak filtering of binary output.
;; `beardbolt-asm-format': Choose between intel att, and other syntax if supported.
;; `beardbolt-demangle': Demangle the output, if supported.
;;
;; For more advanced configuration (to the point where you can override almost
;; all of beardbolt yourself), you can set `beardbolt-language-descriptor' with a
;; replacement language spec.
;;
;; Please see the readme at https://gitlab.com/jgkamat/beardbolt for
;; more information!
;;
;; Thanks:
;; Inspiration and some assembly parsing logic was adapted from Matt Godbolt's
;; compiler-explorer: https://github.com/mattgodbolt/compiler-explorer and
;; Jonas Konrad's javap: https://github.com/yawkat/javap.

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
  :safe (lambda (v) (or (booleanp v) (stringp v)))
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
(defcustom bb-filter-directives t
  "Whether to filter assembly directives."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(defcustom bb-filter-labels t
  "Whether to filter unused labels."
  :type 'boolean
  :safe 'booleanp
  :group 'beardbolt)
(defcustom bb-filter-comment-only t
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
(defcustom bb-flag-quirks t
  "Whether to tweak flags to enable as many features as possible.

In most cases, we will try to honor flags in bb-command as
much as possible. However, some features may be disabled with
some odd combinations of flags. This variable controls
removing/adding flags to handle those cases.

Note that basic flags to ensure basic usage are always modified."
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
(defvar-local bb--dump-file nil "Temporary file")
(defvar-local bb-line-mappings nil "Maps source lines -> asm regions")
(defvar-local bb--relation-overlays nil "Overlays relating source to asm.")
(defvar-local bb--rainbow-overlays nil "Rainbow overlays.")

(defun bb--output-buffer (src-buffer)
  "Get/create output buffer for current source file."
  (with-current-buffer src-buffer
    (or (and (buffer-live-p bb--output-buffer) bb--output-buffer)
        (setq bb--output-buffer
              (with-current-buffer
                  (generate-new-buffer (format "*bb-output for %s*" src-buffer))
                (asm-mode)
                (setq bb--source-buffer src-buffer)
                (bb--output-mode)
                (current-buffer))))))

;; whether bb-mode is enabled.
(defvar bb-hide-compile t)

(defvar bb-binary-asm-limit 10000)

(defvar bb-compile-delay 0.4
  "Time in seconds to delay before recompiling if there is a change.")

(defvar bb--shell "bash"
  "Which shell to prefer if available.
Used to work around inconsistencies in alternative shells.")

(defvar bb--temp-dir nil
  "Temporary directory to use for compilation and other reasons.")

(defun bb--temp-dir ()
  (or (and bb--temp-dir
           (file-exists-p bb--temp-dir)
           bb--temp-dir)
      (setq bb--temp-dir (make-temp-file "beardbolt-bb-" t)))) 

(defvar bb-dir nil
  "The directory which beardbolt is installed to.")
(when load-file-name
  (setq bb-dir (file-name-directory load-file-name)))

(defvar-local bb-objdump-binary "objdump"
  "A binary to use for objdumping when using `bb-disassemble'.
Useful if you have multiple objdumpers and want to select between them")

;;;; Variable-like funcs
(defun bb-output-filename (src-buffer &optional asm)
  "Function for generating an output filename for SRC-BUFFER.

Outputs assembly file if ASM.
This function does NOT quote the return value for use in inferior shells."
  (if (and (not asm)
           (buffer-local-value 'bb-disassemble src-buffer))
      (expand-file-name "beardbolt.out" (bb--temp-dir))
    (expand-file-name "beardbolt.s" (bb--temp-dir))))

;;;; Regexes

(defvar bb-label-def  (rx bol (group (any ".a-zA-Z_$@")
                                          (0+ (any "a-zA-Z0-9$_@.")))
                               ":"))
(defvar bb-defines-global (rx bol (0+ space) ".glob"
                                   (opt "a") "l" (0+ space)
                                   (group (any ".a-zA-Z_")
                                          (0+ (any "a-zA-Z0-9$_.")))))
(defvar bb-label-find (rx (any ".a-zA-Z_")
                               (0+
                                (any "a-zA-Z0-9$_."))))
(defvar bb-assignment-def (rx bol (0+ space)
                                   (group (any ".a-zA-Z_$")
                                          (1+ (any "a-zA-Z0-9$_.")))
                                   (0+ space) "="))
(defvar bb-has-opcode (rx bol (0+ space)
                               (any "a-zA-Z")))

(defvar bb-defines-function (rx bol (0+ space) ".type"
                                     (0+ any) "," (0+ space) (any "@%")
                                     "function" eol))
(defvar bb-data-defn (rx bol (0+ space) "."
                              (group (or "string" "asciz" "ascii"
                                         (and
                                          (optional (any "1248")) "byte")
                                         "short" "word" "long" "quad" "value" "zero"))))

(defvar bb-directive (rx bol (0+ space) "." (0+ any) eol))
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
(defvar bb-source-file (rx bol (0+ space) ".file" (1+ space)
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

;;;; Classes

(cl-defstruct (bb-lang
               (:constructor make-beardbolt-lang)
               (:conc-name bb--lang-))
  (objdumper
   nil :documentation "Object dumper to use if disassembling binary.")
  (demangler
   nil :documentation "If non-nil, demangler to use for this source code")
  (base-cmd
   nil :documentation "")
  (compile-cmd-function
   nil :documentation "")
  (asm-function
   nil :documentation "Function to operate on an assembly listing")
  (disass-function
   nil :documentation "Function to operate on a binary")
  (disass-hidden-funcs
   nil :documentation "Regexp recognizing non-user assembly rountines."))

(defmacro bb--set-local (var val)
  "Set unquoted variable VAR to value VAL in current buffer."
  (declare (debug (symbolp form)))
  `(set (make-local-variable ,var) ,val))

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

;;;; Language Functions
;;;;; Compile Commands

(cl-defun bb--c-compile-cmd ()
  "Process a compile command for gcc/clang."
  (let* ((cmd (or bb-command
                  (bb--lang-base-cmd (bb--get-lang))))
         (cmd (mapconcat #'identity
                         (list cmd
                               "-g"
                               (if bb-disassemble "-c" "-S")
                               (cond ((derived-mode-p 'c++-mode) "-x c++")
                                     (t "-x c"))
                               "-"
                               "-o" (shell-quote-argument (bb-output-filename
                                                           (current-buffer)))
                               (when (and bb-asm-format
                                          (not bb-disassemble))
                                 (format "-masm=%s" bb-asm-format)))
                         " "))
         (cmd (if (and bb-flag-quirks
                       (string-match-p (rx "-save-temps") cmd)
                       (string-match-p (rx "-P") cmd))
                  (bb-split-rm-single cmd "-save-temps")
                cmd)))
    cmd))

;;;;; Hidden Function Definitions

(defvar bb--hidden-func-c
  (rx bol (or (and "__" (0+ any))
              (and "_" (or "init" "start" "fini"))
              (and (opt "de") "register_tm_clones")
              "call_gmon_start"
              "frame_dummy"
              (and ".plt" (0+ any)))
      eol))

;;;; Language Definitions
(defvar bb-languages
  `((c-mode
     . ,(make-beardbolt-lang :compile-cmd-function #'bb--c-compile-cmd
                             :objdumper 'objdump
                             :asm-function #'bb--process-src-asm-lines
                             :disass-function #'bb--process-disassembled-lines
                             :demangler "c++filt"
                             :disass-hidden-funcs bb--hidden-func-c))
    (c++-mode
     . ,(make-beardbolt-lang :compile-cmd-function #'bb--c-compile-cmd
                             :objdumper 'objdump
                             :asm-function #'bb--process-src-asm-lines
                             :disass-function #'bb--process-disassembled-lines
                             :demangler "c++filt"
                             :disass-hidden-funcs bb--hidden-func-c))))

;;;; Macros

(defmacro bb-with-display-buffer-no-window (&rest body)
  "Run BODY without displaying any window."
  ;; See http://debbugs.gnu.org/13594
  `(let ((display-buffer-overriding-action
          (if bb-hide-compile
              (list #'display-buffer-no-window)
            display-buffer-overriding-action)))
     ,@body))

;;;; Functions
;; Functions to parse and lint assembly were lifted almost directly from the compiler-explorer

(defun bb-re-seq (regexp string)
  "Get list of all REGEXP match in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

;;;;; Filter Functions

;; Filtering functions were more or less lifted from the godbolt
;; compiler explorer to maintain compatiblity.
;; https://github.com/mattgodbolt/compiler-explorer/blob/master/lib/asm.js

(defun bb--has-opcode-p (line)
  "Check if LINE has opcodes."
  (save-match-data
    (let* ((match (string-match bb-label-def line))
           (line (if match
                     (substring line (match-end 0))
                   line))
           (line (cl-first (split-string line (rx (1+ (any ";#")))))))
      (if (string-match-p bb-assignment-def line)
          nil
        (string-match-p bb-has-opcode line)))))

(defun bb--find-used-labels (src-buffer asm-lines)
  "Find used labels in ASM-LINES generated from SRC-BUFFER."
  (let ((match nil)
        (current-label nil)
        (labels-used (make-hash-table :test #'equal))
        (weak-usages (make-hash-table :test #'equal)))
    (dolist (line asm-lines)
      (setq line (string-trim-left line)
            match (and (string-match bb-label-def line)
                       (match-string 1 line)))
      (when match
        (setq current-label match))
      (setq match (and (string-match bb-defines-global line)
                       (match-string 1 line)))
      (when match
        (puthash match t labels-used))
      ;; When we have no line or a period started line, skip
      (unless (or (string-empty-p line)
                  (eq (elt line 0) ?.)
                  (not (string-match-p bb-label-find line)))
        (if (or (not (buffer-local-value 'bb-filter-directives src-buffer))
                (bb--has-opcode-p line)
                (string-match-p bb-defines-function line))
            ;; Add labels indescriminantly
            (dolist (l (bb-re-seq bb-label-find line))
              (puthash l t labels-used))
          (when (and current-label
                     (or (string-match-p bb-data-defn line)
                         (bb--has-opcode-p line)))
            (dolist (l (bb-re-seq bb-label-find line))
              (cl-pushnew l (gethash current-label weak-usages) :test #'equal))))))

    (let* ((max-label-iter 10)
           (label-iter 0)
           (completed nil))

      (while (and (<= (cl-incf label-iter)
                      max-label-iter)
                  (not completed))
        (let ((to-add nil))
          (maphash
           (lambda (label _v)
             (dolist (now-used (gethash label weak-usages))
               (when (not (gethash now-used labels-used))
                 (cl-pushnew now-used to-add :test #'equal))))
           labels-used)
          (if to-add
              (dolist (l to-add)
                (puthash l t labels-used))
            (setq completed t))))
      labels-used)))

(defun bb--user-func-p (src-buffer func)
  "Return t if FUNC is a user function.
Argument SRC-BUFFER source buffer."
  (let* ((lang (with-current-buffer src-buffer (bb--get-lang)))
         (regexp (bb--lang-disass-hidden-funcs lang)))
    (if regexp (not (string-match-p regexp func)) t)))

;; TODO godbolt does not handle disassembly with filter=off, but we should.
(cl-defun bb--process-disassembled-lines (src-buffer asm-lines)
  "Process and filter disassembled ASM-LINES from SRC-BUFFER."
  (let* ((src-file-name "<stdin>")
         (result nil)
         (func nil)
         (source-linum nil))
    (dolist (line asm-lines)
      (catch 'continue
        (when (and (> (length result) bb-binary-asm-limit)
                   (not (buffer-local-value 'bb-ignore-binary-limit src-buffer)))
          (cl-return-from bb--process-disassembled-lines
            '("Aborting processing due to exceeding the binary limit.")))
        (when (string-match bb-disass-line line)
          ;; Don't add linums from files which we aren't inspecting
          (if (equal src-file-name
                     (file-name-base (match-string 1 line)))
              (setq source-linum (string-to-number (match-string 2 line)))
            (setq source-linum nil))
          ;; We are just setting a linum, no data here.
          (throw 'continue t))

        (when (string-match bb-disass-label line)
          (setq func (match-string 2 line))
          (when (bb--user-func-p src-buffer func)
            (push (concat func ":") result))
          (throw 'continue t))
        (unless (and func
                     (bb--user-func-p src-buffer func))
          (throw 'continue t))
        (when (string-match bb-disass-opcode line)
          (let ((line (concat (match-string 1 line)
                              "\t" (match-string 3 line))))
            ;; Add line text property if available
            (when source-linum
              (add-text-properties 0 (length line)
                                   `(bb-src-line ,source-linum) line))
            (push line result))
          (throw 'continue t))))
    (nreverse result)))

(cl-defun bb--process-src-asm-lines (src-buffer asm-lines)
  (let* ((used-labels (bb--find-used-labels src-buffer asm-lines))
         (src-file-name "<stdin>")
         (result nil)
         (prev-label nil)
         (source-linum nil)
         (source-file-map (make-hash-table :test #'eq)))
    (dolist (line asm-lines)
      (let* ((raw-match (or (string-match bb-label-def line)
                            (string-match bb-assignment-def line)))
             (match (when raw-match
                      (match-string 1 line)))
             (used-label-p (gethash match used-labels)))
        (catch 'continue
          (cond
           ;; Process file name hints
           ((string-match bb-source-file line)
            (if (match-string 3 line)
                ;; Clang style match
                (puthash (string-to-number (match-string 1 line))
                         (expand-file-name (match-string 3 line) (match-string 2 line))
                         source-file-map)
              (puthash (string-to-number (match-string 1 line))
                       (match-string 2 line)
                       source-file-map)))
           ;; Process any line number hints
           ((string-match bb-source-tag line)
            (if (equal src-file-name
                       (gethash
                        (string-to-number (match-string 1 line))
                        source-file-map
                        ;; Assume we never will compile dev null :P
                        "/dev/null"))
                (setq source-linum (string-to-number
                                    (match-string 2 line)))
              (setq source-linum nil)))
           ((string-match bb-source-stab line)
            (pcase (string-to-number (match-string 1 line))
              ;; http://www.math.utah.edu/docs/info/stabs_11.html
              (68
               (setq source-linum (match-string 2 line)))
              ((or 100 132)
               (setq source-linum nil)))))
          ;; End block, reset prev-label and source
          (when (string-match-p bb-endblock line)
            (setq prev-label nil))

          (when (and (buffer-local-value 'bb-filter-comment-only src-buffer)
                     (string-match-p bb-comment-only line))
            (throw 'continue t))

          ;; continue means we don't add to the ouptut
          (when match
            (if (not used-label-p)
                ;; Unused label
                (when (buffer-local-value 'bb-filter-labels src-buffer)
                  (throw 'continue t))
              ;; Real label, set prev-label
              (setq prev-label raw-match)))
          (when (and (buffer-local-value 'bb-filter-directives src-buffer)
                     (not match))
            (if  (and (string-match-p bb-data-defn line)
                      prev-label)
                ;; data is being used
                nil
              (when (string-match-p bb-directive line)
                (throw 'continue t))))
          ;; Add line numbers to mapping
          (when (and source-linum
                     (bb--has-opcode-p line))
            (add-text-properties 0 (length line)
                                 `(bb-src-line ,source-linum) line))
          ;; Add line
          (push line result))))
    (nreverse result)))

(defun bb--rainbowize (line-mappings src-buffer)
  (let* ((background-hsl
          (apply #'color-rgb-to-hsl (color-name-to-rgb (face-background 'default))))
         all-ovs
         (idx 0)
         ;; The 1+ helps us keep our hue distance from the actual
         ;; background color
         (total (1+ (hash-table-count line-mappings))))
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
     line-mappings)
 (mapc #'delete-overlay bb--rainbow-overlays)
    (setq-local bb--rainbow-overlays all-ovs)))

(defun bb--delete-rainbow-overlays ()
  (mapc #'delete-overlay bb--rainbow-overlays)
  (setq bb--rainbow-overlays nil))

(defun bb--make-line-mappings (lines)
  (let ((linum 1)
        (start-match nil)
        (in-match nil)
        (ht (make-hash-table)))
    (dolist (line lines)
      (let ((property (get-text-property 0 'bb-src-line line)))
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
      (cl-incf linum))
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
(cl-defun bb--handle-finish-compile (compilation-buffer str &key override-buffer)
  "Finish hook for compilations.  Runs in buffer COMPILATION-BUFFER.
Argument STR compilation finish status.
Argument OVERRIDE-BUFFER asm src buffer to use instead of reading
   `bb-output-filename'."
  (delete-file bb--dump-file)
  (let ((src-buffer bb--source-buffer))
    (with-current-buffer (bb--output-buffer src-buffer)
      ;; Store src buffer value for later linking
      (cond
       ((string-match "^finished" str)
        (if (and (not override-buffer)
                 (not (file-exists-p (bb-output-filename src-buffer t))))
            (message "Error reading from output file.")
          (let* ((lang (with-current-buffer src-buffer (bb--get-lang)))
                 (unfiltered-lines
                  (or (when override-buffer
                        (with-current-buffer override-buffer
                          (split-string (buffer-string) "\n" nil)))
                      (with-temp-buffer
                        (insert-file-contents (bb-output-filename src-buffer t))
                        (split-string (buffer-string) "\n" nil))))
                 (lines
                  (funcall (if (with-current-buffer src-buffer bb-disassemble)
                               (bb--lang-disass-function lang)
                             (bb--lang-asm-function lang))
                           src-buffer unfiltered-lines)))
            (display-buffer (current-buffer) '(nil (inhibit-same-window . t)))
            ;; Replace buffer contents but save point and scroll
            (let* ((window (get-buffer-window))
                   (old-point (and window (window-point window)))
                   (old-window-start (and window (window-start window))))
              (erase-buffer)
              (insert (mapconcat #'identity lines "\n"))
              (when window
                (set-window-start window old-window-start)
                (set-window-point window old-point)))
            (setq bb-line-mappings (bb--make-line-mappings lines))
            (bb--rainbowize bb-line-mappings src-buffer))))
       (t
        ;; Display compilation buffer
        (display-buffer compilation-buffer '(nil (inhibit-same-window . t)))
        ;; output no longer up-to-date, kill output buffer
        ;; immediately.  This also tears down relation overlays
        ;; in the source buffer, if any.
        (kill-buffer (current-buffer)))))))

;;;;; Parsing Options
(defvar-local bb--language-descriptor nil)
(defun bb--get-lang ()
  "Helper function to get lang def for LANGUAGE."
  (or bb--language-descriptor
      (cdr (assoc major-mode bb-languages))))

(defun bb--demangle-command (existing-cmd lang src-buffer)
  "Append a demangler routine to EXISTING-CMD with LANG and SRC-BUFFER
and return it."
  (if-let ((to-demangle (buffer-local-value 'bb-demangle src-buffer))
           (demangler (bb--lang-demangler lang))
           (demangler-exists (executable-find demangler)))
      (concat existing-cmd " "
              (mapconcat
               #'identity
               (list "&&" demangler
                     "<" (bb-output-filename src-buffer t)
                     ">" (expand-file-name "tmp.s" (bb--temp-dir))
                     "&&" "mv"
                     (expand-file-name "tmp.s" (bb--temp-dir))
                     (bb-output-filename src-buffer t))
               " "))
    existing-cmd))

(cl-defmacro bb--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

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
                 (error "[beardbolt] Some variables unsafe %s" unsafe-vars))
               (when risky-vars
                 (error "[beardbolt] Some variables risky %s" risky-vars)))))
    (hack-local-variables))
  (let* ((dump-file
          (make-temp-file "beardbolt-dump-" nil
                          (concat "." (file-name-extension buffer-file-name))
                          (buffer-string)))
         (src-buffer (current-buffer))
         (func (bb--lang-compile-cmd-function lang))
         (cmd (concat (funcall func) " < " dump-file)))
    (when bb-disassemble
      (pcase (bb--lang-objdumper lang)
        ('objdump
         (setq cmd
               (mapconcat #'identity
                          (list cmd
                                "&&"
                                bb-objdump-binary "-d" (bb-output-filename src-buffer)
                                "-C" "--insn-width=16" "-l"
                                (when (not (booleanp bb-asm-format))
                                  (concat "-M " bb-asm-format))
                                ">" (bb-output-filename src-buffer t))
                          " ")))
        (_
         (error "Objdumper not recognized"))))
    ;; Convert to demangle if we need to
    (setq cmd (bb--demangle-command cmd lang src-buffer))
    (with-current-buffer ; With compilation buffer
        (let ((shell-file-name (or (executable-find bb--shell)
                                   shell-file-name))
              (compilation-auto-jump-to-first-error t))
          ;; TODO should this be configurable?
          (bb-with-display-buffer-no-window
           (compilation-start cmd nil #'bb--compilation-buffer)))
      ;; Only jump to errors, skip over warnings
      (setq-local compilation-skip-threshold 2)
      (setq-local inhibit-message t)
      (add-hook 'compilation-finish-functions
                #'bb--handle-finish-compile nil t)
      (setq bb--source-buffer src-buffer)
      (setq bb--dump-file dump-file))))

(defun bb--maybe-stop-running-compilation ()
  (let ((buffer (bb--compilation-buffer)))
    (when-let ((proc (get-buffer-process buffer)))
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

(defun bb--synch-relation-overlays (mapping source-line)
  "Update overlays to visually match selected source and asm lines.
Runs in output buffer.  Sets `bb--relation-overlays'."
  (bb--delete-relation-overlays)
  (setq bb--relation-overlays nil)
  (let (ovs
        (positions (plist-get (gethash source-line mapping) :positions)))
    (when positions
      (bb--when-live-buffer bb--source-buffer
        (save-excursion
          (push
           (progn
             (goto-char (point-min))
             (bb--make-relation-overlay
              (line-beginning-position source-line)
              (line-end-position source-line)))
           ovs))
        (bb--recenter-maybe (overlay-start (car ovs))))
      (cl-loop for (start . end) in positions
               do (push (bb--make-relation-overlay start end) ovs)
               finally (bb--recenter-maybe (caar positions)))
      (setq bb--relation-overlays ovs))))

(defun bb--delete-relation-overlays ()
  (mapc #'delete-overlay bb--relation-overlays)
  (setq bb--relation-overlays nil))

(defun bb--source-buffer-pch ()
  (let ((linum (line-number-at-pos nil t)))
    (bb--when-live-buffer bb--output-buffer
      (bb--delete-relation-overlays)
      (bb--synch-relation-overlays bb-line-mappings linum))))

(defun bb--on-kill-source-buffer ()
  (bb--when-live-buffer bb--output-buffer
    (kill-buffer bb--output-buffer)))

(defun bb--on-kill-output-buffer ()
  (bb--delete-relation-overlays)
  (bb--delete-rainbow-overlays))

(defun bb--output-buffer-pch ()
  (bb--delete-relation-overlays)
  (bb--synch-relation-overlays
   bb-line-mappings
   (get-text-property (point) 'bb-src-line)))

(defvar bb--change-timer nil)

(defun bb--after-change (&rest _)
  (bb--when-live-buffer bb--output-buffer
    (clrhash bb-line-mappings))
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
