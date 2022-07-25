;;; beardbolt-test.el --- Tests for beardbolt  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for beardbolt

;;; Code:

(require 'el-mock nil t)
(require 'beardbolt)

(ert-deftest sanity-check-ert ()
  "Check if ERT is working. :)"
  (should t))

(defun test-asm-preprocessor (pre post)
  "Tests the asm preprocessor on the current buffer."
  (insert-file-contents pre)
  (should
   (string=
    (string-trim
     (mapconcat 'identity
                (beardbolt--process-asm-lines (current-buffer)
                                            (split-string (buffer-string) "\n" t))
                "\n"))
    (with-temp-buffer
      (insert-file-contents post)
      (string-trim
       (buffer-string))))))

;;;; Filtration tests

(ert-deftest filter-tests-all-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local beardbolt-disassemble nil)
    (setq-local beardbolt-filter-comment-only t)
    (setq-local beardbolt-filter-directives t)
    (setq-local beardbolt-filter-labels t)
    (test-asm-preprocessor "test/beardbolt-c-pre1.s" "test/beardbolt-c-post1.s")))
(ert-deftest filter-tests-none-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local beardbolt-disassemble nil)
    (setq-local beardbolt-filter-comment-only nil)
    (setq-local beardbolt-filter-directives nil)
    (setq-local beardbolt-filter-labels nil)
    (test-asm-preprocessor "test/beardbolt-c-pre1.s" "test/beardbolt-c-post2.s")))
(ert-deftest filter-tests-dir-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local beardbolt-disassemble nil)
    (setq-local beardbolt-filter-comment-only nil)
    (setq-local beardbolt-filter-directives t)
    (setq-local beardbolt-filter-labels nil)
    (test-asm-preprocessor "test/beardbolt-c-pre1.s" "test/beardbolt-c-post3.s")))
(ert-deftest filter-tests-weak-ref-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local beardbolt-disassemble nil)
    (setq-local beardbolt-filter-comment-only nil)
    (setq-local beardbolt-filter-directives t)
    (setq-local beardbolt-filter-labels t)
    (test-asm-preprocessor "test/beardbolt-c-pre2.s" "test/beardbolt-c-post4.s")))

;;;; Demangler tests

(ert-deftest demangler-test-disabled ()
  (with-temp-buffer
    (setq-local beardbolt-demangle nil)
    (should
     (string-empty-p
      (beardbolt--demangle-command
       ""
       (make-beardbolt-lang :demangler nil)
       (current-buffer))))))

(ert-deftest demangler-test-invalid-demangler ()
  (with-temp-buffer
    (setq-local beardbolt-demangle t)
    (should
     (string-empty-p
      (beardbolt--demangle-command
       ""
       (make-beardbolt-lang :demangler nil)
       (current-buffer))))))

(ert-deftest demangler-test-not-path ()
  (with-temp-buffer
    (setq-local beardbolt-demangle t)
    (should
     (string-empty-p
      (beardbolt--demangle-command
       ""
       (make-beardbolt-lang :demangler "nonsense-binary-name-not-on-path")
       (current-buffer))))))

(ert-deftest demangler-test-valid-demangler ()
  ;; Assumes test is on the path!
  (with-temp-buffer
    (setq-local beardbolt-demangle t)
    (should
     (string-match-p
      (regexp-opt '("test"))
      (beardbolt--demangle-command
       ""
       (make-beardbolt-lang :demangler "test")
       (current-buffer))))))


;;; beardbolt-test.el ends here
(provide 'beardbolt-test)
