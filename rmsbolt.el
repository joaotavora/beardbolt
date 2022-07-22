;;; rmsbolt.el --- A compiler output viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021 Jay Kamat
;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Version: 0.1.2
;; Keywords: compilation, tools
;; URL: http://gitlab.com/jgkamat/rmsbolt
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

;; RMSBolt is a package to provide assembly or bytecode output for a source
;; code input file.
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
;; RMSBolt is primarily configured with Emacs local variables. This lets you
;; change compiler and rmsbolt options simply by editing a local variable block.
;;
;; Notable options:
;; `rmsbolt-command': determines the prefix of the compilation command to use.
;; `rmsbolt-default-directory': determines the default-drectory to compile from.
;; `rmsbolt-disassemble': disassemble from a compiled binary with objdump, if supported.
;; `rmsbolt-filter-*': Tweak filtering of binary output.
;; `rmsbolt-asm-format': Choose between intel att, and other syntax if supported.
;; `rmsbolt-demangle': Demangle the output, if supported.
;;
;; For more advanced configuration (to the point where you can override almost
;; all of RMSbolt yourself), you can set `rmsbolt-language-descriptor' with a
;; replacement language spec.
;;
;; Please see the readme at https://gitlab.com/jgkamat/rmsbolt for
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
(require 'cc-defs)
(require 'compile)
(require 'disass)
(require 'json)
(require 'color)

(require 'rmsbolt-java)
(require 'rmsbolt-split)

;;; Code:
;;;; Customize:
(defgroup rmsbolt nil
  "rmsbolt customization options"
  :group 'applications)

(defcustom rmsbolt-use-overlays t
  "Whether we should use overlays to show matching code."
  :type 'boolean
  :group 'rmsbolt)
(defcustom rmsbolt-goto-match t
  "Whether we should goto the match in the other buffer if it is non visible."
  :type 'boolean
  :group 'rmsbolt)
(defcustom rmsbolt-mode-lighter " RMS🗲"
  "Lighter displayed in mode line when function `rmsbolt-mode' is active."
  :type 'string
  :group 'rmsbolt)
(defcustom rmsbolt-large-buffer-size 500
  "Number of lines past which a buffer is considred large."
  :type 'integer
  :group 'rmsbolt)
(defcustom rmsbolt-automatic-recompile t
  "Whether to automatically save and recompile the source buffer.
This setting is automatically disabled on large buffers, set to
`force' to force-enable it.  To only recompile when the buffer is
manually saved, set to `on-save'."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On save" on-save)
                 (const :tag "On" t)
                 (const :tag "Always" force))
  :group 'rmsbolt)

;;;;; Buffer Local Tweakables
(defcustom rmsbolt-disassemble nil
  "Whether we should disassemble an output binary."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-command nil
  "The base command to run rmsbolt from."
  :type 'string
  ;; nil means use default command
  :safe (lambda (v) (or (booleanp v) (stringp v)))
  :group 'rmsbolt)
(defcustom rmsbolt-default-directory nil
  "The default directory to compile from.
This must be an absolute path if set.
Some exporters (such as pony) may not work with this set."
  :type 'string
  ;; nil means use default command
  :safe (lambda (v) (or (booleanp v) (stringp v)))
  :group 'rmsbolt)
(define-obsolete-variable-alias 'rmsbolt-intel-x86
  'rmsbolt-asm-format "RMSBolt-0.2"
  "Sorry about not providing a proper migration for this variable.
Unfortunately the new options aren't a straightforward mapping.
Most likely what you want:

t -> \"intel\"
nil -> \"att\"
tool defaults -> nil

This means that if you had rmsbolt-intel-x86 set manually, you
are now getting tool defaults.")
(defcustom rmsbolt-asm-format "intel"
  "Which output assembly format to use.

The supported values depend highly on the exporter, but typical
values are: intel, att, <nil/t> (for using tool defaults).
Invalid values will be passed onto the disassembly tools, which
may throw errors.

If you are not on x86, you most likely want to set this to nil.

Since this defaults to \"intel\", implementers must support this
being set (at worst falling back to nil if passed \"intel\")."
  :type 'string
  :safe (lambda (v) (or (booleanp v) (stringp v)))
  :group 'rmsbolt)
(defcustom rmsbolt-filter-directives t
  "Whether to filter assembly directives."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-filter-labels t
  "Whether to filter unused labels."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-filter-comment-only t
  "Whether to filter comment-only lines."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-ignore-binary-limit nil
  "Whether to ignore the binary limit. Could hang emacs..."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-demangle t
  "Whether to attempt to demangle the resulting assembly."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-flag-quirks t
  "Whether to tweak flags to enable as many features as possible.

In most cases, we will try to honor flags in rmsbolt-command as
much as possible. However, some features may be disabled with
some odd combinations of flags. This variable controls
removing/adding flags to handle those cases.

Note that basic flags to ensure basic usage are always modified."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)

(defcustom rmsbolt-after-parse-hook nil
  "Hook after all parsing is done, but before compile command is run.

Exercise caution when setting variables in this hook - doing so
can disrupt rmsbolt state and cause issues. Variables set here
may not be cleared to default as variables are usually."
  :group 'rmsbolt
  :type 'hook)

;;;; Faces

(defface rmsbolt-current-line-face
  '((t (:weight bold :inherit highlight)))
  "Face to fontify the current line for showing matches."
  :group 'rmsbolt)

;;;; Variables:
(defvar rmsbolt-output-buffer "*rmsbolt-output*")
;; whether rmsbolt-mode is enabled.
(defvar rmsbolt-mode)

(defvar rmsbolt-hide-compile t)
(defvar rmsbolt-binary-asm-limit 10000)
(defvar-local rmsbolt-line-mapping nil
  "Line mapping hashtable from source lines -> asm lines")
(defvar-local rmsbolt-current-line nil
  "Current line for fontifier.")
(defvar-local rmsbolt--last-point nil
  "Used to detect when the point has moved.")

(defvar rmsbolt-overlays nil
  "List of overlays to use.")
(defvar-local rmsbolt--rainbow-overlays nil
  "List of rainbow overlays to use.")
(defvar rmsbolt-compile-delay 0.4
  "Time in seconds to delay before recompiling if there is a change.")
(defvar rmsbolt--automated-compile nil
  "Whether this compile was automated or not.")
(defvar rmsbolt--shell "bash"
  "Which shell to prefer if available.
Used to work around inconsistencies in alternative shells.")

(defvar rmsbolt--temp-dir nil
  "Temporary directory to use for compilation and other reasons.

Please DO NOT modify this blindly, as this directory will get
deleted on Emacs exit.")

(defvar rmsbolt-dir nil
  "The directory which rmsbolt is installed to.")
(when load-file-name
  (setq rmsbolt-dir (file-name-directory load-file-name)))

(defvar-local rmsbolt-src-buffer nil)

(defvar-local rmsbolt--real-src-file nil
  "If set, the real filename that we compiled from,
probably due to a copy from this file.")
;; FIXME should we be unbinding the list here, or is setting nil good enough.
(defvar-local rmsbolt--default-variables nil
  "A list of the buffer-local variables we filled in with defaults.
Useful for determining if the user overrode things like `rmsbolt-command'.

This list of variables will automatically be restored to nil.")

(defvar-local rmsbolt-objdump-binary "objdump"
  "A binary to use for objdumping when using `rmsbolt-disassemble'.
Useful if you have multiple objdumpers and want to select between them")

;;;; Variable-like funcs
(defun rmsbolt-output-filename (src-buffer &optional asm)
  "Function for generating an output filename for SRC-BUFFER.

Outputs assembly file if ASM.
This function does NOT quote the return value for use in inferior shells."
  (if (and (not asm)
           (buffer-local-value 'rmsbolt-disassemble src-buffer))
      (expand-file-name "rmsbolt.out" rmsbolt--temp-dir)
    (expand-file-name "rmsbolt.s" rmsbolt--temp-dir)))

;;;; Regexes

(defvar rmsbolt-label-def  (rx bol (group (any ".a-zA-Z_$@")
                                          (0+ (any "a-zA-Z0-9$_@.")))
                               ":"))
(defvar rmsbolt-defines-global (rx bol (0+ space) ".glob"
                                   (opt "a") "l" (0+ space)
                                   (group (any ".a-zA-Z_")
                                          (0+ (any "a-zA-Z0-9$_.")))))
(defvar rmsbolt-label-find (rx (any ".a-zA-Z_")
                               (0+
                                (any "a-zA-Z0-9$_."))))
(defvar rmsbolt-assignment-def (rx bol (0+ space)
                                   (group (any ".a-zA-Z_$")
                                          (1+ (any "a-zA-Z0-9$_.")))
                                   (0+ space) "="))
(defvar rmsbolt-has-opcode (rx bol (0+ space)
                               (any "a-zA-Z")))

(defvar rmsbolt-defines-function (rx bol (0+ space) ".type"
                                     (0+ any) "," (0+ space) (any "@%")
                                     "function" eol))
(defvar rmsbolt-data-defn (rx bol (0+ space) "."
                              (group (or "string" "asciz" "ascii"
                                         (and
                                          (optional (any "1248")) "byte")
                                         "short" "word" "long" "quad" "value" "zero"))))

(defvar rmsbolt-directive (rx bol (0+ space) "." (0+ any) eol))
(defvar rmsbolt-endblock (rx "." (or "cfi_endproc" "data" "text" "section")))
(defvar rmsbolt-comment-only (rx bol (0+ space) (or (and (or (any "#@;") "//"))
                                                    (and "/*" (0+ any) "*/"))
                                 (0+ any) eol))
(defvar rmsbolt-disass-line (rx bol
                                (group "/" (1+ (not (any ":")))) ":"
                                (group (1+ num))
                                (0+ any)))
(defvar rmsbolt-disass-label (rx bol (group (1+ (any digit "a-f")))
                                 (1+ space) "<"
                                 (group (1+ (not (any ">")))) ">:" eol))
(defvar rmsbolt-disass-dest (rx (0+ any) (group (1+ (any digit "a-f")))
                                (1+ space) "<" (group (1+ (not (any ">")))) ">" eol))

(defvar rmsbolt-disass-opcode (rx bol (0+ space) (group (1+ (any digit "a-f")))
                                  ":" (0+ space)
                                  (group (1+
                                          (repeat 2
                                                  (any digit "a-f"))
                                          (opt " ")))
                                  (0+ space)
                                  (group (0+ any))))
(defvar rmsbolt-source-file (rx bol (0+ space) ".file" (1+ space)
                                (group (1+ digit)) (1+ space) ?\"
                                (group (1+ (not (any ?\")))) ?\"
                                (opt (1+ space) ?\"
                                     (group (1+ (not (any ?\")))) ?\")
                                (0+ any)))
(defvar rmsbolt-source-tag (rx bol (0+ space) ".loc" (1+ space)
                               (group (1+ digit)) (1+ space)
                               (group (1+ digit))
                               (0+ any)))
(defvar rmsbolt-source-stab (rx bol (0+ any) ".stabn" (1+ space)
                                (group (1+ digit)) ",0,"
                                (group (1+ digit)) "," (0+ any)))

;;;; Classes

(cl-defstruct (rmsbolt-lang
               (:conc-name rmsbolt-l-))
  (supports-disass
   nil
   :type 'bool
   :documentation "If we support assembly directly. If nil, we must use other methods.")
  (supports-asm
   nil
   :type 'bool
   :documentation "If we support disassembling from binaries. If nil, we must use other methods.")
  (objdumper
   'objdump
   :type 'symbol
   :documentation "The object dumper to use if disassembling binary.")
  (demangler
   nil
   :type 'string
   :documentation "The command of the demangler to use for this source code.
If nil, don't demangle.")
  (disass-hidden-funcs
   nil
   :type 'string
   :documentation "Functions that are hidden when disassembling.")
  (compile-cmd
   nil
   :type 'string
   :documentation "Default compilation command to use if none is provided.
If provided a function, call that function with the source buffer to determine
the compile command.")
  (default-directory
    nil
    :type 'string
    :documentation "Default directory to run compilation in. By default, use rmsbolt--temp-dir.
If provided a function, call that function with the source buffer to determine
the default directory.")
  (compile-cmd-function
   nil
   :type 'function
   :documentation "A function which takes in a compile command
(could be the default) and adds needed args to it.")
  (process-asm-custom-fn
   nil
   :type 'function
   :documentation "A custom function used for parsing asm lines
   instead of the default assembly one." )
  (elisp-compile-override
   nil
   :type 'function
   :documentation "A custom function to run instead of running any compilation command.
Generally not useful with the sole exception of the emacs lisp disassembler.
This function is responsible for calling `rmsbolt--handle-finish-compile'
Please be careful when setting this, as it bypasses most logic and is
generally not useful."))

;;;; Helper Functions
(defmacro rmsbolt--with-files (src-buffer &rest body)
  "Execute BODY with `src-filename' and `output-filename' defined.
Args taken from SRC-BUFFER.
Return value is quoted for passing to the shell."
  `(let ((src-filename (shell-quote-argument
                        (buffer-file-name)))
         (output-filename
          (shell-quote-argument
           (rmsbolt-output-filename ,src-buffer))))
     ,@body))

(defmacro rmsbolt--set-local (var val)
  "Set unquoted variable VAR to value VAL in current buffer."
  (declare (debug (symbolp form)))
  `(set (make-local-variable ,var) ,val))

;;;; Language Functions
;;;;; Compile Commands

(defun rmsbolt--c-quirks (cmd &key src-buffer)
  "Handle quirks in CMD, and return unchanged or modified CMD.

Use SRC-BUFFER as buffer for local variables."
  (if (and (buffer-local-value 'rmsbolt-flag-quirks src-buffer)
           (string-match-p (rx "-save-temps") cmd)
           (string-match-p (rx "-P") cmd))
      (rmsbolt-split-rm-single cmd "-save-temps")
    cmd))

(cl-defun rmsbolt--c-compile-cmd (&key src-buffer)
  "Process a compile command for gcc/clang."

  (rmsbolt--with-files
   src-buffer
   (let* ( ;; Turn off passing the source file if we find compile_commands
          (no-src-filename (rmsbolt--handle-c-compile-cmd src-buffer))
          (asm-format (buffer-local-value 'rmsbolt-asm-format src-buffer))
          (disass (buffer-local-value 'rmsbolt-disassemble src-buffer))
          (cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
                                "-g"
                                (if disass
                                    "-c"
                                  "-S")
                                (if no-src-filename
                                    ""
                                  src-filename)
                                "-o" output-filename
                                (when (and (not (booleanp asm-format))
                                           (not disass))
                                  (concat "-masm=" asm-format)))
                          " "))
          (cmd (rmsbolt--c-quirks cmd :src-buffer src-buffer)))
     cmd)))

(cl-defun rmsbolt--ocaml-compile-cmd (&key src-buffer)
  "Process a compile command for ocaml.

  Needed as ocaml cannot output asm to a non-hardcoded file"
  (rmsbolt--with-files
   src-buffer
   (let* ((diss (buffer-local-value 'rmsbolt-disassemble src-buffer))
          (predicted-asm-filename (shell-quote-argument
                                   (concat (file-name-sans-extension (buffer-file-name)) ".s")))
          (cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
                                "-g"
                                (if (buffer-local-value 'rmsbolt-disassemble src-buffer)
                                    ""
                                  "-S")
                                src-filename
                                (mapconcat #'identity
                                           (cond
                                            (diss
                                             (list "-o" output-filename))
                                            ((equal predicted-asm-filename output-filename)
                                             nil)
                                            (t
                                             (list "&&" "mv"
                                                   predicted-asm-filename
                                                   output-filename)))
                                           " "))
                          " ")))
     cmd)))
(cl-defun rmsbolt--lisp-compile-cmd (&key src-buffer)
  "Process a compile command for common lisp.

   Assumes function name to disassemble is \\='main\\='."
  (rmsbolt--with-files
   src-buffer
   (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (interpreter (cl-first (split-string cmd nil t)))
          (disass-eval "\"(disassemble 'main)\"")
          (disass-eval-unquoted "(disassemble 'main)"))
     (pcase interpreter
       ("sbcl"
        (mapconcat #'identity
                   (list cmd "--noinform" "--load"
                         src-filename
                         "--eval" disass-eval "--non-interactive"
                         ;; Remove leading comments
                         "|" "sed" "'s/^;\s//'" ">"
                         output-filename)
                   " "))
       ("clisp"
        (mapconcat #'identity
                   (list cmd "-q" "-x"
                         (concat
                          "\"(load \\\"" src-filename "\\\") " disass-eval-unquoted "\"")
                         ">" output-filename)
                   " "))
       (_
        (error "This Common Lisp interpreter is not supported"))))))
(cl-defun rmsbolt--rust-compile-cmd (&key src-buffer)
  "Process a compile command for rustc."
  (rmsbolt--with-files
   src-buffer
   (let* ((asm-format (buffer-local-value 'rmsbolt-asm-format src-buffer))
          (disass (buffer-local-value 'rmsbolt-disassemble src-buffer))
          (cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
                                "-g"
                                "--emit"
                                (if disass
                                    "link"
                                  "asm")
                                src-filename
                                "-o" output-filename
                                (when (and (not (booleanp asm-format))
                                           (not disass))
                                  (concat "-Cllvm-args=--x86-asm-syntax=" asm-format)))
                          " ")))
     cmd)))
(cl-defun rmsbolt--go-compile-cmd (&key src-buffer)
  "Process a compile command for go."
  (rmsbolt--with-files
   src-buffer
   (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
                                "tool" "compile"
                                "-S"
                                "-o" output-filename
                                src-filename)
                          " ")))
     cmd)))
(cl-defun rmsbolt--d-compile-cmd (&key src-buffer)
  "Process a compile command for d"
  (rmsbolt--with-files
   src-buffer
   (let* ((compiler (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat
                #'identity
                (list compiler "-g" "-output-s" src-filename "-of" output-filename)
                " ")))
     cmd)))

(cl-defun rmsbolt--pony-compile-cmd (&key src-buffer)
  "Process a compile command for ponyc."
  (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (dir (expand-file-name "pony/" rmsbolt--temp-dir))
         (_ (make-directory dir t))
         ;; (base-filename (file-name-sans-extension
         ;;                 (file-name-nondirectory
         ;;                  (buffer-file-name))))
         (base-filename "pony")
         (base-filename (expand-file-name base-filename dir))
         (asm-filename (shell-quote-argument (concat base-filename ".s")))
         (object-filename (shell-quote-argument (concat base-filename ".o")))
         ;; TODO should we copy this in lisp here, or pass this to the compilation command?
         (_ (copy-file (buffer-file-name)
                       (expand-file-name dir) t))
         (dis (buffer-local-value 'rmsbolt-disassemble src-buffer))
         (cmd (mapconcat #'identity
                         (list
                          "cd" dir "&&"
                          cmd
                          "-g"
                          ;; FIXME: test this properly and use rmsbolt-asm-format to expose it.
                          (if dis
                              "-r=obj"
                            "-r=asm")
                          dir
                          "&&" "mv"
                          (if dis object-filename asm-filename)
                          (shell-quote-argument
                           (rmsbolt-output-filename src-buffer)))
                         " ")))
    (with-current-buffer src-buffer
      (setq rmsbolt--real-src-file
            (expand-file-name (file-name-nondirectory
                               (buffer-file-name))
                              dir)))
    cmd))
(cl-defun rmsbolt--py-compile-cmd (&key src-buffer)
  "Process a compile command for python3."
  (rmsbolt--with-files
   src-buffer
   (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer)))
     (mapconcat #'identity
                (list cmd "-m" "dis" src-filename
                      ">" output-filename)
                " "))))

(defun rmsbolt--hack-p (src-buffer)
  "Return non-nil if SRC-BUFFER should should use hhvm instead of php."
  (with-current-buffer src-buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (rx "<?hh") nil t))))

(defun rmsbolt--php-default-compile-cmd (src-buffer)
  "Return the default php compile command for SRC-BUFFER."
  (if (rmsbolt--hack-p src-buffer)
      "hh_single_compile"
    "php"))

(cl-defun rmsbolt--php-compile-cmd (&key src-buffer)
  "Process a compile command for PHP.
In order to disassemble opcdoes, we need to have the vld.so
extension to php on.
https://github.com/derickr/vld"
  (rmsbolt--with-files
   src-buffer
   (if (rmsbolt--hack-p src-buffer)
       (concat (buffer-local-value 'rmsbolt-command src-buffer)
               " " src-filename " > " output-filename)
     (concat (buffer-local-value 'rmsbolt-command src-buffer)
             " -dvld.active=1 -dvld.execute=0 -dvld.verbosity=1 "
             src-filename " 2> " output-filename " > /dev/null"))))

(cl-defun rmsbolt--hs-compile-cmd (&key src-buffer)
  "Process a compile command for ghc."
  (rmsbolt--with-files
   src-buffer
   (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
                                "-g"
                                (if (buffer-local-value 'rmsbolt-disassemble src-buffer)
                                    ""
                                  "-S")
                                src-filename
                                "-o" output-filename)
                          " ")))
     cmd)))
(cl-defun rmsbolt--java-compile-cmd (&key src-buffer)
  "Process a compile command for ocaml.

  Needed as ocaml cannot output asm to a non-hardcoded file"
  (rmsbolt--with-files
   src-buffer
   (let* ((class-filename (shell-quote-argument
                           (concat (file-name-sans-extension (buffer-file-name)) ".class")))
          (cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
                                "-g"
                                src-filename
                                "&&"
                                "javap"
                                "-c" "-l"
                                class-filename
                                ">"
                                output-filename)
                          " ")))
     cmd)))

(cl-defun rmsbolt--elisp-compile-override (&key src-buffer)
  (let ((file-name (buffer-file-name)))
    (with-temp-buffer
      (rmsbolt--disassemble-file file-name (current-buffer))
      (rmsbolt--handle-finish-compile src-buffer nil :override-buffer (current-buffer)))))

(cl-defun rmsbolt--nim-compile-cmd (&key src-buffer)
  "Process a compile command for nim."
  (rmsbolt--with-files
   src-buffer
   (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
	  (cmd
	   (let* ((outdir (expand-file-name "nim-cache" rmsbolt--temp-dir)))
		  (string-join
		   (list cmd
			 "--debugger:native"
			 "--noLinking"
			 "--colors:off"
			 (concat "--nimcache:" outdir)
			 src-filename
			 (concat "&& cp "
				 (expand-file-name (concat "@m"
							   (file-name-nondirectory src-filename)
							   (if (string-match (rx "nim cpp") cmd) ".cpp.o" ".c.o"))
							   outdir)
				 " " output-filename))
		   " "))))
     cmd)))

(cl-defun rmsbolt--zig-compile-cmd (&key src-buffer)
  "Process a compile command for zig."
  (rmsbolt--with-files
   src-buffer
   (let* ((disass (buffer-local-value 'rmsbolt-disassemble src-buffer))
          (cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (string-join
                (list cmd
                      src-filename
                      "--cache-dir" (expand-file-name "zig-cache" rmsbolt--temp-dir)
                      (concat (if disass
                                  "-femit-bin="
                                "-fno-emit-bin -femit-asm=")
                              output-filename))
                " ")))
     cmd)))

(cl-defun rmsbolt--swift-compile-cmd (&key src-buffer)
  "Process a compile command for swiftc."
  (rmsbolt--with-files
   src-buffer
   (let* ((asm-format (buffer-local-value 'rmsbolt-asm-format src-buffer))
          (cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
                                "-g"
                                "-emit-assembly"
                                src-filename
                                "-o" output-filename
                                (when (not (booleanp asm-format))
                                  (concat "-Xllvm --x86-asm-syntax=" asm-format)))
                          " ")))
     cmd)))

;;;;; Hidden Function Definitions

(defvar rmsbolt--hidden-func-c
  (rx bol (or (and "__" (0+ any))
              (and "_" (or "init" "start" "fini"))
              (and (opt "de") "register_tm_clones")
              "call_gmon_start"
              "frame_dummy"
              (and ".plt" (0+ any)))
      eol))
(defvar rmsbolt--hidden-func-ocaml
  (rx bol
      (or (and "__" (0+ any))
          (and "_" (or "init" "start" "fini"))
          (and (opt "de") "register_tm_clones")
          (and ".plt" (0+ any))
          (and "camlCamlinternalFormat" (0+ any))
          (and (1+ (not (any "@"))) "@plt")
          (and (or "caml_" "camlStd_") (0+ any))
          (and "caml" (or "Pervasives" "List" "Bytes"
                          "String" "Buffer" "Printf"
                          "Char" "Sys") "__" (0+ any))
          ;; Ocaml likes to make labels following camlModule__,
          ;; filter out any lowercase
          (and (1+ (1+ lower) (opt (or "64" "32" "8" "16")) (opt "_"))))
      eol))
(defvar rmsbolt--hidden-func-zig
  (rx bol (or (and "_" (0+ any))
              (and (opt "de") "register_tm_clones")
              "call_gmon_start"
              "frame_dummy"
              (and (0+ any) "@plt" (0+ any)))
      eol))

;;;;; Demangling Functions

(defun rmsbolt--path-to-swift-demangler ()
  "Return the path to the configured Swift demangler, depending
  on the active toolchain."
  (rmsbolt--path-to-swift-tool "swift-demangle"))

;;;;; Language Integrations

(defun rmsbolt--path-to-swift-compiler ()
  "Return the path to the configured Swift compiler, depending on
  the active toolchain."
  (rmsbolt--path-to-swift-tool "swiftc"))

(defun rmsbolt--path-to-swift-tool (swift-tool)
  "Return the path to SWIFT-TOOL, depending on the active
toolchain."
  (let* ((swift-tool-binary swift-tool)
         (swift-tool-toolchain-path (shell-command-to-string (format "echo -n `xcrun --find %s`" swift-tool-binary))))
    ;; If we have the Swift tool in PATH, just return it (this is the
    ;; typical case in Linux systems). If it's not in PATH, look for a
    ;; toolchain-specific path.
    (cond
     ((executable-find swift-tool-binary)
      swift-tool-binary)
     ((executable-find swift-tool-toolchain-path)
      swift-tool-toolchain-path)
     (t nil))))

(defun rmsbolt--parse-compile-commands (comp-cmds file)
  "Parse COMP-CMDS and extract a compilation dir and command for FILE."
  (when-let ((json-object-type 'alist)
             (json-array-type 'vector)
             (cmds (json-read-file comp-cmds))
             (entry (cl-find-if
                     (lambda (elt)
                       (file-equal-p
                        file
                        (expand-file-name
                         (alist-get 'file elt "")
                         (alist-get 'directory elt ""))))
                     cmds))
             (dir (alist-get 'directory entry))
             (cmd (alist-get 'command entry)))
    (list dir cmd)))
(defun rmsbolt--handle-c-compile-cmd (src-buffer)
  "Handle compile_commands.json for c/c++ for a given SRC-BUFFER.
return t if successful."
  (when-let ((defaults (buffer-local-value 'rmsbolt--default-variables src-buffer))
             (default-dir (cl-find 'rmsbolt-default-directory defaults))
             (default-cmd (cl-find 'rmsbolt-command defaults))
             (ccj "compile_commands.json")
             (compile-cmd-file
              (locate-dominating-file
               (buffer-file-name src-buffer)
               ccj))
             (compile-cmd-file (expand-file-name ccj compile-cmd-file))
             (to-ret (rmsbolt--parse-compile-commands
                      compile-cmd-file (buffer-file-name src-buffer))))
    (with-current-buffer src-buffer
      (setq-local rmsbolt-default-directory (file-name-as-directory (cl-first to-ret)))
      (setq-local rmsbolt-command
                  ;; Remove -c, -S, and -o <arg> if present,
                  ;; as we will add them back
                  ;; Remove args starting with -flto, as -flto breaks asm output.
                  (thread-first (cl-second to-ret)
                    (rmsbolt-split-rm-single "-c")
                    (rmsbolt-split-rm-single "-S")
                    (rmsbolt-split-rm-single "-flto" #'string-prefix-p)
                    (rmsbolt-split-rm-double "-o")))
      t)))
;;;; Language Definitions
(defvar rmsbolt-languages)
(setq
 rmsbolt-languages
 `((c-mode
    . ,(make-rmsbolt-lang :compile-cmd "gcc"
                          :supports-asm t
                          :supports-disass t
                          :demangler "c++filt"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-c))
   (c++-mode
    . ,(make-rmsbolt-lang :compile-cmd "g++"
                          :supports-asm t
                          :supports-disass t
                          :demangler "c++filt"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-c))
   (d-mode
    . ,(make-rmsbolt-lang :compile-cmd "ldc2"
                          :supports-asm t
                          :supports-disass nil
                          :demangler "ddemangle"
                          :compile-cmd-function #'rmsbolt--d-compile-cmd))
   ;; In order to parse ocaml files, you need the emacs ocaml mode, tuareg
   (tuareg-mode
    . ,(make-rmsbolt-lang :compile-cmd "ocamlopt"
                          :supports-asm t
                          :supports-disass t
                          :compile-cmd-function #'rmsbolt--ocaml-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-ocaml))
   (lisp-mode
    . ,(make-rmsbolt-lang :compile-cmd "sbcl"
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'cat
                          :compile-cmd-function #'rmsbolt--lisp-compile-cmd))
   (rust-mode
    . ,(make-rmsbolt-lang :compile-cmd "rustc"
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'objdump
                          :demangler "rustfilt"
                          :compile-cmd-function #'rmsbolt--rust-compile-cmd))
   ;; Copy of above
   (rustic-mode
    . ,(make-rmsbolt-lang :compile-cmd "rustc"
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'objdump
                          :demangler "rustfilt"
                          :compile-cmd-function #'rmsbolt--rust-compile-cmd))
   (ponylang-mode
    . ,(make-rmsbolt-lang :compile-cmd "ponyc"
                          :supports-asm t
                          :supports-disass t
                          :objdumper 'objdump
                          :compile-cmd-function #'rmsbolt--pony-compile-cmd))
   (php-mode
    . ,(make-rmsbolt-lang :compile-cmd #'rmsbolt--php-default-compile-cmd
                          :supports-asm t
                          :supports-disass nil
                          :compile-cmd-function #'rmsbolt--php-compile-cmd
                          :process-asm-custom-fn #'rmsbolt--process-php-bytecode))
   ;; ONLY SUPPORTS PYTHON 3
   (python-mode
    . ,(make-rmsbolt-lang :compile-cmd "python3"
                          :supports-asm t
                          :supports-disass nil
                          :compile-cmd-function #'rmsbolt--py-compile-cmd
                          :process-asm-custom-fn #'rmsbolt--process-python-bytecode))
   (haskell-mode
    . ,(make-rmsbolt-lang :compile-cmd "ghc"
                          :supports-asm t
                          :supports-disass nil
                          :demangler "haskell-demangler"
                          :compile-cmd-function #'rmsbolt--hs-compile-cmd))
   (java-mode
    . ,(make-rmsbolt-lang :compile-cmd "javac"
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'cat
                          :compile-cmd-function #'rmsbolt--java-compile-cmd
                          :process-asm-custom-fn #'rmsbolt--process-java-bytecode))
   (emacs-lisp-mode
    . ,(make-rmsbolt-lang :supports-asm t
                          :supports-disass nil
                          ;; Nop
                          :process-asm-custom-fn (lambda (_src-buffer lines)
                                                   lines)
                          :elisp-compile-override #'rmsbolt--elisp-compile-override))
   (nim-mode
    . ,(make-rmsbolt-lang :compile-cmd "nim c"
                          :supports-disass t
                          :objdumper 'objdump
                          :demangler "c++filt"
                          :compile-cmd-function #'rmsbolt--nim-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-c))
   (zig-mode
    . ,(make-rmsbolt-lang :compile-cmd "zig build-obj -O ReleaseFast"
                          :supports-asm t
                          :supports-disass t
                          :objdumper 'objdump
                          :compile-cmd-function #'rmsbolt--zig-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-zig))
   (go-mode
    . ,(make-rmsbolt-lang :compile-cmd "go"
			                    :supports-asm nil
			                    :supports-disass t
			                    :objdumper 'go-objdump
			                    :compile-cmd-function #'rmsbolt--go-compile-cmd
			                    :process-asm-custom-fn #'rmsbolt--process-go-asm-lines))
   (swift-mode
    . ,(make-rmsbolt-lang :compile-cmd (rmsbolt--path-to-swift-compiler)
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'objdump
                          :demangler (rmsbolt--path-to-swift-demangler)
                          :compile-cmd-function #'rmsbolt--swift-compile-cmd))
   ))
(make-obsolete-variable 'rmsbolt-languages
                        'rmsbolt-language-descriptor "RMSBolt-0.2")

(defvar-local rmsbolt-language-descriptor nil
  ;; FIXME: Major modes can't set this without calling `make-rmsbolt-lang',
  ;; so it forces them to require `rmsbolt', which is a bummer.
  "Description of the language tools of current buffer for use by RMSBolt.
This should be an object of type `rmsbolt-lang', normally set by the major mode")

;;;; Macros

(defmacro rmsbolt-with-display-buffer-no-window (&rest body)
  "Run BODY without displaying any window."
  ;; See http://debbugs.gnu.org/13594
  `(let ((display-buffer-overriding-action
          (if rmsbolt-hide-compile
              (list #'display-buffer-no-window)
            display-buffer-overriding-action)))
     ,@body))


;;;; Functions
;; Functions to parse and lint assembly were lifted almost directly from the compiler-explorer

(defun rmsbolt-re-seq (regexp string)
  "Get list of all REGEXP match in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

;; Prevent byte-compilation warnings for cl-print-compiled, which is imported
;; from cl-print
(defvar cl-print-compiled)
(defun rmsbolt--disassemble-file (filename out-buffer)
  "Disassemble an elisp FILENAME into elisp bytecode in OUT-BUFFER.
Lifted from https://emacs.stackexchange.com/questions/35936/disassembly-of-a-bytecode-file"
  (if (not (require 'cl-print nil 'noerror))
      (error "Package cl-print or Emacs 26+ are required for the Emacs disassembler")
    (byte-compile-file filename)
    ;; .el -> .elc
    (setq filename (concat filename "c"))
    (with-temp-buffer
      (insert-file-contents filename)
      (let ((inbuf (current-buffer)))
        (goto-char (point-min))
        (with-current-buffer out-buffer
          (erase-buffer)
          (setq-local cl-print-compiled 'disassemble)
          (condition-case ()
              (cl-loop for expr = (read inbuf)
                       do
                       (pcase expr
                         (`(byte-code ,(pred stringp) ,(pred vectorp) ,(pred natnump))
                          (princ "TOP-LEVEL byte code:\n" (current-buffer))
                          (disassemble-1 expr 0))
                         (_ (cl-prin1 expr (current-buffer))))
                       do (terpri (current-buffer)))
            (end-of-file nil)))))))

;;;;; Filter Functions

;; Filtering functions were more or less lifted from the godbolt compiler explorer to maintain compatiblity.
;; https://github.com/mattgodbolt/compiler-explorer/blob/master/lib/asm.js

(defun rmsbolt--has-opcode-p (line)
  "Check if LINE has opcodes."
  (save-match-data
    (let* ((match (string-match rmsbolt-label-def line))
           (line (if match
                     (substring line (match-end 0))
                   line))
           (line (cl-first (split-string line (rx (1+ (any ";#")))))))
      (if (string-match-p rmsbolt-assignment-def line)
          nil
        (string-match-p rmsbolt-has-opcode line)))))

(defun rmsbolt--find-used-labels (src-buffer asm-lines)
  "Find used labels in ASM-LINES generated from SRC-BUFFER."
  (let ((match nil)
        (current-label nil)
        (labels-used (make-hash-table :test #'equal))
        (weak-usages (make-hash-table :test #'equal)))
    (dolist (line asm-lines)
      (setq line (string-trim-left line)
            match (and (string-match rmsbolt-label-def line)
                       (match-string 1 line)))
      (when match
        (setq current-label match))
      (setq match (and (string-match rmsbolt-defines-global line)
                       (match-string 1 line)))
      (when match
        (puthash match t labels-used))
      ;; When we have no line or a period started line, skip
      (unless (or (string-empty-p line)
                  (eq (elt line 0) ?.)
                  (not (string-match-p rmsbolt-label-find line)))
        (if (or (not (buffer-local-value 'rmsbolt-filter-directives src-buffer))
                (rmsbolt--has-opcode-p line)
                (string-match-p rmsbolt-defines-function line))
            ;; Add labels indescriminantly
            (dolist (l (rmsbolt-re-seq rmsbolt-label-find line))
              (puthash l t labels-used))
          (when (and current-label
                     (or (string-match-p rmsbolt-data-defn line)
                         (rmsbolt--has-opcode-p line)))
            (dolist (l (rmsbolt-re-seq rmsbolt-label-find line))
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

(defun rmsbolt--user-func-p (src-buffer func)
  "Return t if FUNC is a user function.
Argument SRC-BUFFER source buffer."
  (let* ((lang (with-current-buffer src-buffer
                 (rmsbolt--get-lang)))
         (regexp (rmsbolt-l-disass-hidden-funcs lang)))
    (if regexp
        (not (string-match-p regexp func))
      t)))

;; TODO godbolt does not handle disassembly with filter=off, but we should.
(cl-defun rmsbolt--process-disassembled-lines (src-buffer asm-lines)
  "Process and filter disassembled ASM-LINES from SRC-BUFFER."
  (let* ((src-file-name
          (or (buffer-local-value 'rmsbolt--real-src-file src-buffer)
              (buffer-file-name src-buffer)))
         (result nil)
         (func nil)
         (source-linum nil)
         (def-dir (or (buffer-local-value 'rmsbolt-default-directory src-buffer)
                      (and src-file-name
                           (file-name-directory src-file-name)))))
    (dolist (line asm-lines)
      (catch 'continue
        (when (and (> (length result) rmsbolt-binary-asm-limit)
                   (not (buffer-local-value 'rmsbolt-ignore-binary-limit src-buffer)))
          (cl-return-from rmsbolt--process-disassembled-lines
            '("Aborting processing due to exceeding the binary limit.")))
        (when (string-match rmsbolt-disass-line line)
          ;; Don't add linums from files which we aren't inspecting
          ;; If we get a non-absolute .file path, check to see if we
          ;; have a default dir. If not, treat it like we are in the
          ;; src directory.
          (let ((default-directory def-dir))
            (if (file-equal-p src-file-name
                              (match-string 1 line))
                (setq source-linum (string-to-number (match-string 2 line)))
              (setq source-linum nil)))
          ;; We are just setting a linum, no data here.
          (throw 'continue t))

        (when (string-match rmsbolt-disass-label line)
          (setq func (match-string 2 line))
          (when (rmsbolt--user-func-p src-buffer func)
            (push (concat func ":") result))
          (throw 'continue t))
        (unless (and func
                     (rmsbolt--user-func-p src-buffer func))
          (throw 'continue t))
        (when (string-match rmsbolt-disass-opcode line)
          (let ((line (concat (match-string 1 line)
                              "\t" (match-string 3 line))))
            ;; Add line text property if available
            (when source-linum
              (add-text-properties 0 (length line)
                                   `(rmsbolt-src-line ,source-linum) line))
            (push line result))
          (throw 'continue t))))
    (nreverse result)))

(cl-defun rmsbolt--process-src-asm-lines (src-buffer asm-lines)
  (let* ((used-labels (rmsbolt--find-used-labels src-buffer asm-lines))
         (src-file-name (or (buffer-local-value 'rmsbolt--real-src-file src-buffer)
                            (buffer-file-name src-buffer)))
         (result nil)
         (prev-label nil)
         (source-linum nil)
         (source-file-map (make-hash-table :test #'eq))
         (def-dir (or (buffer-local-value 'rmsbolt-default-directory src-buffer)
                      (and src-file-name
                           (file-name-directory src-file-name)))))
    (dolist (line asm-lines)
      (let* ((raw-match (or (string-match rmsbolt-label-def line)
                            (string-match rmsbolt-assignment-def line)))
             (match (when raw-match
                      (match-string 1 line)))
             (used-label-p (gethash match used-labels)))
        (catch 'continue
          (cond
           ;; Process file name hints
           ((string-match rmsbolt-source-file line)
            (if (match-string 3 line)
                ;; Clang style match
                (puthash (string-to-number (match-string 1 line))
                         (expand-file-name (match-string 3 line) (match-string 2 line))
                         source-file-map)
              (puthash (string-to-number (match-string 1 line))
                       (match-string 2 line)
                       source-file-map)))
           ;; Process any line number hints
           ((string-match rmsbolt-source-tag line)
            (if (or (not src-file-name) ;; Skip file match if we don't have a current filename
                    ;; If we get a non-absolute .file path, check to see if we
                    ;; have a default dir. If not, treat it like we are in the
                    ;; src directory.
                    (let ((default-directory def-dir))
                      (file-equal-p src-file-name
                                    (gethash
                                     (string-to-number (match-string 1 line))
                                     source-file-map
                                     ;; Assume we never will compile dev null :P
                                     "/dev/null"))))
                (setq source-linum (string-to-number
                                    (match-string 2 line)))
              (setq source-linum nil)))
           ((string-match rmsbolt-source-stab line)
            (pcase (string-to-number (match-string 1 line))
              ;; http://www.math.utah.edu/docs/info/stabs_11.html
              (68
               (setq source-linum (match-string 2 line)))
              ((or 100 132)
               (setq source-linum nil)))))
          ;; End block, reset prev-label and source
          (when (string-match-p rmsbolt-endblock line)
            (setq prev-label nil))

          (when (and (buffer-local-value 'rmsbolt-filter-comment-only src-buffer)
                     (string-match-p rmsbolt-comment-only line))
            (throw 'continue t))

          ;; continue means we don't add to the ouptut
          (when match
            (if (not used-label-p)
                ;; Unused label
                (when (buffer-local-value 'rmsbolt-filter-labels src-buffer)
                  (throw 'continue t))
              ;; Real label, set prev-label
              (setq prev-label raw-match)))
          (when (and (buffer-local-value 'rmsbolt-filter-directives src-buffer)
                     (not match))
            (if  (and (string-match-p rmsbolt-data-defn line)
                      prev-label)
                ;; data is being used
                nil
              (when (string-match-p rmsbolt-directive line)
                (throw 'continue t))))
          ;; Add line numbers to mapping
          (when (and source-linum
                     (rmsbolt--has-opcode-p line))
            (add-text-properties 0 (length line)
                                 `(rmsbolt-src-line ,source-linum) line))
          ;; Add line
          (push line result))))
    (nreverse result)))

(cl-defun rmsbolt--process-php-bytecode (src-buffer asm-lines)
  (if (rmsbolt--hack-p src-buffer)
      asm-lines
    (let ((state 'useless)
          (current-line nil)
          (result nil))
      (dolist (line asm-lines)
        (cl-case state
          ((text)
           (push line result)
           (when (string-match "^-+$" line)
             (setq state 'asm)))
          ((asm)
           (cond
            ((string-empty-p line) (setq state 'useless))
            ((string-match "^ *\\([0-9]+\\) +[0-9]+" line)
             (setq current-line (string-to-number (match-string 1 line)))
             (add-text-properties 0 (length line) `(rmsbolt-src-line ,current-line) line))
            (t
             (add-text-properties 0 (length line) `(rmsbolt-src-line ,current-line) line)))
           (push line result))
          (otherwise
           (when (string-match "^filename:" line)
             (setq state 'text)))))
      (nreverse result))))

(cl-defun rmsbolt--process-python-bytecode (_src-buffer asm-lines)
  (let ((source-linum nil)
        (result nil))
    (dolist (line asm-lines)
      (if (not (string-match (rx bol (repeat 3 (opt space))
                                 (group (opt (1+ digit))) (0+ space)
                                 (group (opt "-->")) (0+ space)
                                 (group (opt ">>")) (0+ space)
                                 (group (1+ digit)) (0+ space)
                                 (group (1+ (or letter "_"))) (0+ space)
                                 (group (opt (1+ digit))) (0+ space)
                                 (group (opt (0+ any))))
                             line))
          ;; just push the var with no linum
          (push line result)
        ;; Grab line numbers
        (unless (string-empty-p (match-string 1 line))
          (setq source-linum
                (string-to-number (match-string 1 line))))
        ;; Reformat line to be more like assembly
        (setq line (mapconcat #'identity
                              (list (match-string 5 line)
                                    (match-string 6 line)
                                    (match-string 7 line))
                              "\t"))
        (when source-linum
          (add-text-properties 0 (length line)
                               `(rmsbolt-src-line ,source-linum) line))
        ;; Add line
        (push line result)))
    (nreverse result)))

(defun rmsbolt--process-java-bytecode (src-buffer asm-lines)
  "Wrapper for easy integration into rmsbolt.
Argument SRC-BUFFER source buffer.
Argument ASM-LINES input lines."
  (rmsbolt-java-process-bytecode
   asm-lines
   (buffer-local-value 'rmsbolt-filter-directives src-buffer)))

(cl-defun rmsbolt--process-asm-lines (src-buffer asm-lines)
  "Process and filter a set of asm lines."
  (let* ((lang (with-current-buffer src-buffer
                 (rmsbolt--get-lang)))
         (process-asm-fn (when lang
                           (rmsbolt-l-process-asm-custom-fn lang))))
    (cond
     (process-asm-fn
      (funcall process-asm-fn src-buffer asm-lines))
     ((buffer-local-value 'rmsbolt-disassemble src-buffer)
      (rmsbolt--process-disassembled-lines src-buffer asm-lines))
     (t
      (rmsbolt--process-src-asm-lines src-buffer asm-lines)))))

(cl-defun rmsbolt--process-go-asm-lines (_src-buffer asm-lines)
  (let ((source-linum nil)
        (result nil))
    (dolist (line asm-lines)
      (if (not
	   (string-match (rx bol (repeat 2 space)
			     (group (opt (0+ any))) ":"
			     (group (opt (1+ digit)) (1+ "\t"))
			     (group (opt "0x" (0+ hex)) (1+ "\t"))
			     (group (1+ xdigit) (1+ "\t"))
			     (group (opt (0+ any)) (1+ "\t")))
			 line))
          ;; just push the var with no linum
          (push line result)
        ;; Grab line numbers
        (unless (string-empty-p (match-string 2 line))
          (setq source-linum
                (string-to-number (match-string 2 line))))
        ;; Reformat line to be more like assembly
        (setq line (mapconcat #'identity
                              (list (match-string 3 line)
                                    (match-string 4 line)
                                    (match-string 5 line))
                              "\t"))
        (when source-linum
          (add-text-properties 0 (length line)
                               `(rmsbolt-src-line ,source-linum) line))
        ;; Add line
        (push line result)))
    (nreverse result)))

(defun rmsbolt--rainbowize (idx total src-buffer src-line asm-regions)
  (let* ((background-hsl
          (apply #'color-rgb-to-hsl (color-name-to-rgb (face-background 'default))))
         (color (apply #'color-rgb-to-hex
                       (color-hsl-to-rgb (/ (* 1.0 idx) total)
                                         (cl-second background-hsl)
                                         (cl-third background-hsl))))
         all-ovs
         _src-ov)
    (save-excursion
      (cl-loop
       for (beg . end) in (cl-sort asm-regions #'< :key #'car)
       for asm-ov =
       (progn
         (goto-char (point-min)) ;; TODO: could optimize
         (make-overlay (progn (forward-line (1- beg))
                              (line-beginning-position))
                       (progn (forward-line (- end beg))
                              (line-end-position))))
       do
       (overlay-put asm-ov 'priority 0)
       (push asm-ov all-ovs)
       (overlay-put asm-ov 'face `(:background ,color))))
    (when asm-regions
      (with-current-buffer src-buffer
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- src-line))
          (let ((ov (make-overlay (line-beginning-position)
                                  (1+ (line-end-position)))))
            (push ov all-ovs)
            (overlay-put ov 'face `(:background ,color))
            (overlay-put ov 'priority 0)))
        (setq-local rmsbolt--rainbow-overlays
                    (append all-ovs
                            rmsbolt--rainbow-overlays))))))

(defun rmsbolt--rainbowize-cleanup ()
  (mapc #'delete-overlay rmsbolt--rainbow-overlays)
  (setq rmsbolt--rainbow-overlays nil))

;;;;; Handlers
(cl-defun rmsbolt--handle-finish-compile (buffer str &key override-buffer stopped)
  "Finish hook for compilations.
Argument BUFFER compilation buffer.
Argument STR compilation finish status.
Argument OVERRIDE-BUFFER asm src buffer to use instead of reading
   `rmsbolt-output-filename'.
Argument STOPPED The compilation was stopped to start another compilation."
  (when (not (buffer-live-p buffer))
    (error "Dead buffer passed to compilation-finish-function! RMSBolt cannot continue."))
  (let ((compilation-fail
         (and str
              (not (string-match "^finished" str))))
        (default-directory (buffer-local-value 'default-directory buffer))
        (src-buffer (buffer-local-value 'rmsbolt-src-buffer buffer)))

    (with-current-buffer (get-buffer-create rmsbolt-output-buffer)
      ;; Store src buffer value for later linking
      (cond (stopped) ; Do nothing
            ((not compilation-fail)
             (if (and (not override-buffer)
                      (not (file-exists-p (rmsbolt-output-filename src-buffer t))))
                 (message "Error reading from output file.")
               (let ((lines
                      (rmsbolt--process-asm-lines
                       src-buffer
                       (or (when override-buffer
                             (with-current-buffer override-buffer
                               (split-string (buffer-string) "\n" nil)))
                           (with-temp-buffer
                             (insert-file-contents (rmsbolt-output-filename src-buffer t))
                             (split-string (buffer-string) "\n" nil)))))
                     (ht (make-hash-table :test #'eq))
                     (linum 1)
                     (start-match nil)
                     (in-match nil)
                     (output-buffer (current-buffer)))
                 ;; Add lines to hashtable
                 (dolist (line lines)
                   (let ((property
                          (get-text-property
                           0 'rmsbolt-src-line line)))
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
                                (gethash in-match ht))
                          (setq in-match nil
                                start-match nil)
                          (go run-conditional))
                         (property
                          (setq in-match property
                                start-match linum))))))
                   (cl-incf linum))

                 (with-current-buffer src-buffer
                   (setq rmsbolt-line-mapping ht))

                 ;; Replace buffer contents but save point and scroll
                 (let* ((window (get-buffer-window output-buffer))
                        (old-point (window-point window))
                        (old-window-start (window-start window)))
                   (erase-buffer)
                   (insert (mapconcat #'identity lines "\n"))
                   (when window
                     (set-window-start window old-window-start)
                     (set-window-point window old-point)))
                 (asm-mode)
                 (rmsbolt-mode 1)
                 (let ((i 0))
                   (maphash (lambda (k v)
                              (rmsbolt--rainbowize
                               (prog1 i (cl-incf i))
                               (hash-table-count ht)
                               src-buffer
                               k
                               v))
                            ht))
                 (setq rmsbolt-src-buffer src-buffer)
                 (display-buffer (current-buffer))
                 (run-at-time 0 nil #'rmsbolt-update-overlays))))
            (t ; Compilation failed
             ;; Display compilation buffer
             (if rmsbolt--automated-compile
                 (display-buffer buffer)
               ;; If the compilation was directly started by the user,
               ;; select the compilation buffer.
               (pop-to-buffer buffer))
             ;; TODO find a cleaner way to disable overlays.
             (with-current-buffer src-buffer
               (setq rmsbolt-line-mapping nil))
             (rmsbolt--remove-overlays)))
      ;; Reset automated recompile
      (setq rmsbolt--automated-compile nil))
    ;; Clear out default-set variables
    (with-current-buffer src-buffer
      (dolist (var rmsbolt--default-variables)
        (rmsbolt--set-local var nil))
      (setq rmsbolt--default-variables nil))))

;;;;; Parsing Options
(defun rmsbolt--get-lang ()
  "Helper function to get lang def for LANGUAGE."
  (or rmsbolt-language-descriptor
      (cdr-safe (assoc major-mode rmsbolt-languages))))

(defun rmsbolt--parse-options ()
  "Parse RMS options from file."
  (hack-local-variables)
  (let* ((lang (rmsbolt--get-lang))
         (src-buffer (current-buffer))
         (cmd rmsbolt-command)
         (dir rmsbolt-default-directory)
         (force-disass (not (rmsbolt-l-supports-asm lang)))
         (force-asm (not (rmsbolt-l-supports-disass lang))))
    ;; If this is non-nil, most likely we are running two compiles at once.
    ;; This is not exactly ideal, as it causes a race condition.
    (when rmsbolt--default-variables
      (message "It looks like RMSbolt state wasn't cleaned up properly.
Are you running two compilations at the same time?"))
    (when (and force-disass force-asm)
      (error "No disassemble method found for this langauge, please double check spec"))
    (when force-disass
      (setq-local rmsbolt-disassemble t))
    (when force-asm
      (setq-local rmsbolt-disassemble nil))
    (when (not dir)
      (add-to-list 'rmsbolt--default-variables 'rmsbolt-default-directory)
      (setq-local rmsbolt-default-directory
                  (let ((new-dir (rmsbolt-l-default-directory lang)))
                    (pcase new-dir
                      ((pred functionp) (funcall new-dir src-buffer))
                      (_ new-dir)))))
    (when (not cmd)
      (add-to-list 'rmsbolt--default-variables 'rmsbolt-command)
      (setq-local rmsbolt-command
                  (let ((new-cmd (rmsbolt-l-compile-cmd lang)))
                    (pcase new-cmd
                      ((pred functionp) (funcall new-cmd src-buffer))
                      (_ new-cmd)))))
    src-buffer))

(defun rmsbolt--demangle-command (existing-cmd lang src-buffer)
  "Append a demangler routine to EXISTING-CMD with LANG and SRC-BUFFER
and return it."
  (if-let ((to-demangle (buffer-local-value 'rmsbolt-demangle src-buffer))
           (demangler (rmsbolt-l-demangler lang))
           (demangler-exists (executable-find demangler)))
      (concat existing-cmd " "
              (mapconcat
               #'identity
               (list "&&" demangler
                     "<" (rmsbolt-output-filename src-buffer t)
                     ">" (expand-file-name "tmp.s" rmsbolt--temp-dir)
                     "&&" "mv"
                     (expand-file-name "tmp.s" rmsbolt--temp-dir)
                     (rmsbolt-output-filename src-buffer t))
               " "))
    existing-cmd))

;;;;; UI Functions
(defun rmsbolt-compile ()
  "Compile the current rmsbolt buffer."
  (interactive)
  (when (and (buffer-modified-p)
             (yes-or-no-p (format "Save buffer %s? " (buffer-name))))
    (save-buffer))
  (rmsbolt--gen-temp)
  ;; Current buffer = src-buffer at this point
  (setq rmsbolt-src-buffer (current-buffer))
  (cond
   ((eq major-mode 'asm-mode)
    ;; We cannot compile asm-mode files
    (message "Cannot compile assembly files. Are you sure you are not in the output buffer?"))
   ((rmsbolt-l-elisp-compile-override (rmsbolt--get-lang))
    (with-current-buffer (or (buffer-base-buffer) (current-buffer))
      (funcall
       (rmsbolt-l-elisp-compile-override (rmsbolt--get-lang))
       :src-buffer (current-buffer))))
   (t
    (rmsbolt--stop-running-compilation)
    (rmsbolt--parse-options)
    (let* ((src-buffer (current-buffer))
           (lang (rmsbolt--get-lang))
           (func (rmsbolt-l-compile-cmd-function lang))
           ;; Generate command
           (cmd
            ;; Compilation commands assume the current buffer is a real file
            ;; currently - this works around that.
            (with-current-buffer (or (buffer-base-buffer) (current-buffer))
              (funcall func :src-buffer src-buffer)))
           (asm-format
            (buffer-local-value 'rmsbolt-asm-format src-buffer))
           (default-directory (or rmsbolt-default-directory
                                  rmsbolt--temp-dir)))
      (run-hooks 'rmsbolt-after-parse-hook)
      (rmsbolt--rainbowize-cleanup)
      (when (buffer-local-value 'rmsbolt-disassemble src-buffer)
        (pcase
            (rmsbolt-l-objdumper lang)
          ('objdump
           (setq cmd
                 (mapconcat #'identity
                            (list cmd
                                  "&&"
                                  rmsbolt-objdump-binary "-d" (rmsbolt-output-filename src-buffer)
                                  "-C" "--insn-width=16" "-l"
                                  (when (not (booleanp asm-format))
                                    (concat "-M " asm-format))
                                  ">" (rmsbolt-output-filename src-buffer t))
                            " ")))
          ('go-objdump
           (setq cmd
                 (mapconcat #'identity
                            (list cmd
                                  "&&"
                                  "go" "tool"
                                  "objdump" (rmsbolt-output-filename src-buffer)
                                  ">" (rmsbolt-output-filename src-buffer t))
                            " ")))
          ('cat
           (setq cmd
                 (mapconcat #'identity
                            (list cmd
                                  "&&" "mv"
                                  (rmsbolt-output-filename src-buffer)
                                  (rmsbolt-output-filename src-buffer t))
                            " ")))
          (_
           (error "Objdumper not recognized"))))
      ;; Convert to demangle if we need to
      (setq cmd (rmsbolt--demangle-command cmd lang src-buffer))
      (with-current-buffer ; With compilation buffer
          (let ((shell-file-name (or (executable-find rmsbolt--shell)
                                     shell-file-name))
                (compilation-auto-jump-to-first-error t))
            ;; TODO should this be configurable?
            (rmsbolt-with-display-buffer-no-window
             (compilation-start cmd nil (lambda (&rest _) "*rmsbolt-compilation*"))))
        ;; Only jump to errors, skip over warnings
        (setq-local compilation-skip-threshold 2)
        (add-hook 'compilation-finish-functions
                  #'rmsbolt--handle-finish-compile nil t)
        (setq rmsbolt-src-buffer src-buffer))))))

(defun rmsbolt--stop-running-compilation ()
  (when-let* ((compilation-buffer (get-buffer "*rmsbolt-compilation*"))
              (proc (get-buffer-process compilation-buffer)))
    (when (eq (process-status proc) 'run)
      (set-process-sentinel proc nil)
      (interrupt-process proc)
      (rmsbolt--handle-finish-compile compilation-buffer nil :stopped t)
      ;; Wait a short while for the process to exit cleanly
      (sit-for 0.2)
      (delete-process proc))))

;;;; Keymap
(defvar rmsbolt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'rmsbolt-compile)
    map)
  "Keymap for function `rmsbolt-mode'.")

;;;; Init commands

(defun rmsbolt--gen-temp ()
  "Generate rmsbolt temp dir if needed."
  (unless (and rmsbolt--temp-dir
               (file-exists-p rmsbolt--temp-dir))
    (setq rmsbolt--temp-dir
          (make-temp-file "rmsbolt-" t))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (when (and (boundp 'rmsbolt--temp-dir)
                           rmsbolt--temp-dir
                           (file-directory-p rmsbolt--temp-dir))
                  (delete-directory rmsbolt--temp-dir t))
                (setq rmsbolt--temp-dir nil)))))

;;;;; Starter Definitions

;; IIUC, this "starter" business is not a necessary part of RMSBolt, but is
;; a way to provide sample files with which users can try out RMSBolt.

(defvar rmsbolt-starter-files
  '(("c" . "rmsbolt.c")
    ("c++" . "rmsbolt.cpp")
    ("ocaml" . "rmsbolt.ml")
    ("cl" . "rmsbolt.lisp")
    ("rust " . "rmsbolt.rs")
    ("python" . "rmsbolt.py")
    ("haskell" . "rmsbolt.hs")
    ("php" . "rmsbolt.php")
    ("pony" . "rmsbolt.pony")
    ("emacs-lisp" . "rmsbolt-starter.el")
    ("d" . "rmsbolt.d")
    ("zig" . "rmsbolt.zig")
    ("go" . "rmsbolt.go")
    ("swift" . "rmsbolt.swift")
    ;; Rmsbolt is capitalized here because of Java convention of Capitalized
    ;; class names.
    ("java" . "Rmsbolt.java")))

;;;###autoload
(defun rmsbolt-starter (lang-name)
  "Setup new file based on the sample STARTER-FILE-NAME."
  (interactive
   (list (completing-read "Language: " rmsbolt-starter-files nil t)))
  (rmsbolt--gen-temp)
  (let* ((starter-file-name (cdr (assoc lang-name rmsbolt-starter-files)))
         (file-name
          (expand-file-name starter-file-name rmsbolt--temp-dir))
         (exists (file-exists-p file-name))
         (src-file-name
          (when rmsbolt-dir
            (expand-file-name starter-file-name
                              (expand-file-name "starters/" rmsbolt-dir))))
         (src-file-exists (when src-file-name
                            (file-exists-p src-file-name))))
    (if (not src-file-exists)
        (error "Could not find starter files! Are you sure the starter/ folder is available? If you want to overide, set `rmsbolt-dir' to your install path")
      (unless exists
        (copy-file src-file-name file-name))
      (find-file file-name)
      (unless rmsbolt-mode
        (rmsbolt-mode 1)))))

;;;; Overlay Commands
(defun rmsbolt--goto-line (line)
  "Goto a certain LINE."
  (when line
    (let ((cur (line-number-at-pos)))
      (forward-line (- line cur)))))
(defun rmsbolt--setup-overlay (start end buf)
  "Setup overlay with START and END in BUF."
  (let ((o (make-overlay start end buf)))
    (overlay-put o 'face 'rmsbolt-current-line-face)
    (overlay-put o 'priority 1)
    o))
(cl-defun rmsbolt--point-visible (point)
  "Check if the current point is visible in a window in the current buffer."
  (when (cl-find-if (lambda (w)
                      (and (>= point (window-start w))
                           (<= point (window-end w))))
                    (get-buffer-window-list))
    t))

(cl-defun rmsbolt-update-overlays (&key (force nil))
  "Update overlays to highlight the currently selected source and asm lines.
  If FORCE, always scroll overlay, even when one is visible.
  FORCE also scrolls to the first line, instead of the first line
  of the last block."
  (when rmsbolt-mode
    (if-let ((should-run rmsbolt-use-overlays)
             (output-buffer (get-buffer rmsbolt-output-buffer))
             (src-buffer (buffer-local-value 'rmsbolt-src-buffer output-buffer))
             (should-run (and (or (eq (current-buffer) src-buffer)
                                  (eq (current-buffer) output-buffer))
                              ;; Don't run on unsaved buffers
                              (not (buffer-modified-p src-buffer))
                              (buffer-local-value 'rmsbolt-mode src-buffer)))
             (current-line (line-number-at-pos))
             (src-current-line
              (if (eq (current-buffer) src-buffer)
                  current-line
                (get-text-property (point) 'rmsbolt-src-line)))
             (line-mappings (buffer-local-value 'rmsbolt-line-mapping src-buffer))
             (asm-regions (gethash src-current-line line-mappings))
             ;; TODO also consider asm
             (src-pts
              (with-current-buffer src-buffer
                (save-excursion
                  (rmsbolt--goto-line src-current-line)
                  (cl-values (c-point 'bol) (c-point 'bonl))))))
        (let*
            ;; If nil, output-buffer is scrolled instead
            ((scroll-src-buffer-p (not (eq (current-buffer) src-buffer)))
             (line-visible (or (not rmsbolt-goto-match)
                               (when scroll-src-buffer-p
                                 (with-current-buffer src-buffer
                                   (rmsbolt--point-visible (cl-first src-pts)))))))
          ;; Remove existing overlays
          (rmsbolt--remove-overlays)
          (push (rmsbolt--setup-overlay (cl-first src-pts) (cl-second src-pts) src-buffer)
                rmsbolt-overlays)
          (with-current-buffer output-buffer
            (let ((saved-pt (point)))
              (save-excursion
                (cl-loop for (start . end) in asm-regions
                         do (let ((start-pt (progn (rmsbolt--goto-line start)
                                                   (c-point 'bol)))
                                  (end-pt (progn (rmsbolt--goto-line end)
                                                 (c-point 'bonl))))
                              (when (and (not line-visible)
                                         (not scroll-src-buffer-p))
                                (setq line-visible (or (rmsbolt--point-visible start-pt)
                                                       (rmsbolt--point-visible end-pt)
                                                       (and (> saved-pt start-pt)
                                                            (< saved-pt end-pt)))))
                              (push (rmsbolt--setup-overlay start-pt end-pt output-buffer)
                                    rmsbolt-overlays)))))
            (when (or (not line-visible) force)
              ;; Scroll buffer to first line
              (when-let ((scroll-buffer (if scroll-src-buffer-p
                                            src-buffer
                                          output-buffer))
                         (window (get-buffer-window scroll-buffer))
                         (line-scroll (if scroll-src-buffer-p
                                          src-current-line
                                        (progn
                                          (car-safe
                                           ;; If forcing, pick the last region instead
                                           (if force
                                               (car-safe (last asm-regions))
                                             (cl-first asm-regions)))))))
                (with-selected-window window
                  (rmsbolt--goto-line line-scroll)
                  ;; If we scrolled, recenter
                  (recenter))))))
      (rmsbolt--remove-overlays))
    ;; If not in rmsbolt-mode, don't do anything
    ))

(defun rmsbolt--remove-overlays ()
  "Clean up overlays, assuming they are no longer needed."
  (mapc #'delete-overlay rmsbolt-overlays)
  (setq rmsbolt-overlays nil))

(defun rmsbolt--post-command-hook ()
  ;; Use (point) instead of (line-number-at-pos) to track movements because
  ;; the former is faster (constant runtime)
  (unless (eq (point) rmsbolt--last-point)
    (setq rmsbolt--last-point (point))
    (rmsbolt-update-overlays)))

(defun rmsbolt--on-kill-buffer ()
  (when-let (output-buffer (get-buffer rmsbolt-output-buffer))
    (when (or (eq (current-buffer) output-buffer)
              (eq (current-buffer) (buffer-local-value 'rmsbolt-src-buffer output-buffer)))
      (rmsbolt--remove-overlays))))

(defun rmsbolt--is-active-src-buffer ()
  (when-let (output-buffer (get-buffer rmsbolt-output-buffer))
    (eq (current-buffer) (buffer-local-value 'rmsbolt-src-buffer output-buffer))))

(defun rmsbolt--after-save ()
  (when (and (rmsbolt--is-active-src-buffer)
             rmsbolt-automatic-recompile)
    (setq rmsbolt--automated-compile t)
    (rmsbolt-compile)))

;; Auto-save the src buffer after it has been unchanged for `rmsbolt-compile-delay' seconds.
;; The buffer is then automatically recompiled via `rmsbolt--after-save'.
(defvar rmsbolt--change-timer nil)
(defvar rmsbolt--buffer-to-auto-save nil)

(defun rmsbolt--after-change (&rest _)
  (when (and (rmsbolt--is-active-src-buffer)
             rmsbolt-automatic-recompile
             (not (eq rmsbolt-automatic-recompile 'on-save)))
    (when rmsbolt--change-timer
      (cancel-timer rmsbolt--change-timer))
    (setq rmsbolt--buffer-to-auto-save (current-buffer)
          rmsbolt--change-timer (run-with-timer rmsbolt-compile-delay nil #'rmsbolt--on-change-timer))))

(defun rmsbolt--on-change-timer ()
  (setq rmsbolt--change-timer nil)
  (when (buffer-live-p rmsbolt--buffer-to-auto-save)
    (with-current-buffer rmsbolt--buffer-to-auto-save
      (setq rmsbolt--buffer-to-auto-save nil)
      (when (or (< (line-number-at-pos (point-max)) rmsbolt-large-buffer-size)
                (eq rmsbolt-automatic-recompile 'force))
        ;; Clear `before-save-hook' to prevent things like whitespace cleanup
        ;; (e.g., set by spacemacs in `spacemacs-whitespace-cleanup.el`)
        ;; and aggressive indenting from running (this is a hot recompile).
        ;; TODO does anyone want before-save-hook to run on a hot recompile?
        (let ((before-save-hook nil))
          (save-buffer))))))

;;;; Mode Definition:

;;;###autoload
;; TODO handle more modes than c-mode
(define-minor-mode rmsbolt-mode
  "Toggle rmsbolt-mode.

This mode is enabled in both src and assembly output buffers."
  :global nil
  :lighter rmsbolt-mode-lighter
  :keymap rmsbolt-mode-map
  ;; Init
  (cond
   (rmsbolt-mode
    (setq rmsbolt--last-point (point))
    (add-hook 'post-command-hook #'rmsbolt--post-command-hook nil t)
    (add-hook 'kill-buffer-hook #'rmsbolt--on-kill-buffer nil t)

    (when (and rmsbolt-automatic-recompile
               ;; Only turn on auto-save in src buffers
               (not (eq (current-buffer) (get-buffer rmsbolt-output-buffer))))
      (add-hook 'after-save-hook #'rmsbolt--after-save nil t)
      (when (eq rmsbolt-automatic-recompile t)
        (add-hook 'after-change-functions #'rmsbolt--after-change nil t)))

    (rmsbolt--gen-temp))
   (t ;; Cleanup
    (rmsbolt--remove-overlays)
    (remove-hook 'after-change-functions #'rmsbolt--after-change t)
    (remove-hook 'after-save-hook #'rmsbolt--after-save t)
    (remove-hook 'kill-buffer-hook #'rmsbolt--on-kill-buffer t)
    (remove-hook 'post-command-hook #'rmsbolt--post-command-hook t))))

;;;###autoload
(defun rmsbolt ()
  "Start a rmsbolt compilation and enable `rmsbolt-mode' for code region
highlighting and automatic recompilation."
  (interactive)
  (unless rmsbolt-mode
    (rmsbolt-mode))
  (rmsbolt-compile))

(provide 'rmsbolt)

;;; rmsbolt.el ends here
