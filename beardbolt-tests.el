;;; beardbolt-test.el --- Tests for beardbolt  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for beardbolt

;;; Code:

(require 'ert)
(require 'beardbolt)

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

(ert-deftest test-split-single ()
  "Test split single function"
  (should (equal
           (beardbolt-split-rm-single "/usr/bin/c++ -a -R -c" "-R")
           "/usr/bin/c++ -a -c"))

  (should (equal
           (beardbolt-split-rm-single "/usr/bin/c++		-a -R -c" "-R")
           "/usr/bin/c++ -a -c"))

  (should (equal
           (beardbolt-split-rm-single "/usr/bin/c++ -a -R -c" "-a")
           "/usr/bin/c++ -R -c"))

  (should (equal
           (beardbolt-split-rm-single "/usr/bin/c++ -a -R -c" "-c")
           "/usr/bin/c++ -a -R"))

  (should (equal
           (beardbolt-split-rm-single "/usr/bin/c++ -a -R -c" "-z")
           "/usr/bin/c++ -a -R -c"))

  (should (equal
           (beardbolt-split-rm-single "/usr/bin/c++ -a -R -a" "-a")
           "/usr/bin/c++ -R"))

  (should (equal
           (beardbolt-split-rm-single "/usr/bin/c++ -a -R -c -flto=thin" "-flto" #'string-prefix-p)
           "/usr/bin/c++ -a -R -c"))

  (should (equal
           (beardbolt-split-rm-single
            "/usr/bin/c++   -DQT_CORE_LIB -DQT_GUI_LIB -DQT_NETWORK_LIB -DQT_NO_KEYWORDS -DQT_WIDGETS_LIB -Isrc/googletest/include -I../common -Icommon -Icommon/protobuf -isystem /usr/include/x86_64-linux-gnu/qt5 -isystem /usr/include/x86_64-linux-gnu/qt5/QtCore -isystem /usr/lib/x86_64-linux-gnu/qt5/mkspecs/linux-g++-64 -isystem /usr/include/x86_64-linux-gnu/qt5/QtNetwork -isystem /usr/include/x86_64-linux-gnu/qt5/QtWidgets -isystem /usr/include/x86_64-linux-gnu/qt5/QtGui   -Werror=return-local-addr -fPIC -g -g -Og -Werror=return-type -Werror=delete-non-virtual-dtor   -fPIC -std=gnu++14 -o common/CMakeFiles/common.dir/Geometry2d/Arc.cpp.o -c /home/jay/Code/robocup-software/common/Geometry2d/Arc.cpp" "-c")
           "/usr/bin/c++ -DQT_CORE_LIB -DQT_GUI_LIB -DQT_NETWORK_LIB -DQT_NO_KEYWORDS -DQT_WIDGETS_LIB -Isrc/googletest/include -I../common -Icommon -Icommon/protobuf -isystem /usr/include/x86_64-linux-gnu/qt5 -isystem /usr/include/x86_64-linux-gnu/qt5/QtCore -isystem /usr/lib/x86_64-linux-gnu/qt5/mkspecs/linux-g++-64 -isystem /usr/include/x86_64-linux-gnu/qt5/QtNetwork -isystem /usr/include/x86_64-linux-gnu/qt5/QtWidgets -isystem /usr/include/x86_64-linux-gnu/qt5/QtGui -Werror=return-local-addr -fPIC -g -g -Og -Werror=return-type -Werror=delete-non-virtual-dtor -fPIC -std=gnu++14 -o common/CMakeFiles/common.dir/Geometry2d/Arc.cpp.o /home/jay/Code/robocup-software/common/Geometry2d/Arc.cpp")))


(ert-deftest test-split-double ()
  "Test split single function"
  (should (equal
           (beardbolt-split-rm-double "/usr/bin/c++ -a -R -c" "-R")
           "/usr/bin/c++ -a"))

  (should (equal
           (beardbolt-split-rm-double "/usr/bin/c++		-a -R -c" "-c")
           "/usr/bin/c++ -a -R"))

  (should (equal
           (beardbolt-split-rm-double "/usr/bin/c++ -a -R -c" "-a")
           "/usr/bin/c++ -c"))

  (should (equal
           (beardbolt-split-rm-double
            "/usr/bin/c++   -DQT_CORE_LIB -DQT_GUI_LIB -DQT_NETWORK_LIB -DQT_NO_KEYWORDS -DQT_WIDGETS_LIB -Isrc/googletest/include -I../common -Icommon -Icommon/protobuf -isystem /usr/include/x86_64-linux-gnu/qt5 -isystem /usr/include/x86_64-linux-gnu/qt5/QtCore -isystem /usr/lib/x86_64-linux-gnu/qt5/mkspecs/linux-g++-64 -isystem /usr/include/x86_64-linux-gnu/qt5/QtNetwork -isystem /usr/include/x86_64-linux-gnu/qt5/QtWidgets -isystem /usr/include/x86_64-linux-gnu/qt5/QtGui   -Werror=return-local-addr -fPIC -g -g -Og -Werror=return-type -Werror=delete-non-virtual-dtor   -fPIC -std=gnu++14 -o common/CMakeFiles/common.dir/Geometry2d/Arc.cpp.o -c /home/jay/Code/robocup-software/common/Geometry2d/Arc.cpp" "-o")
           "/usr/bin/c++ -DQT_CORE_LIB -DQT_GUI_LIB -DQT_NETWORK_LIB -DQT_NO_KEYWORDS -DQT_WIDGETS_LIB -Isrc/googletest/include -I../common -Icommon -Icommon/protobuf -isystem /usr/include/x86_64-linux-gnu/qt5 -isystem /usr/include/x86_64-linux-gnu/qt5/QtCore -isystem /usr/lib/x86_64-linux-gnu/qt5/mkspecs/linux-g++-64 -isystem /usr/include/x86_64-linux-gnu/qt5/QtNetwork -isystem /usr/include/x86_64-linux-gnu/qt5/QtWidgets -isystem /usr/include/x86_64-linux-gnu/qt5/QtGui -Werror=return-local-addr -fPIC -g -g -Og -Werror=return-type -Werror=delete-non-virtual-dtor -fPIC -std=gnu++14 -c /home/jay/Code/robocup-software/common/Geometry2d/Arc.cpp")))


;;; beardbolt-test.el ends here
(provide 'beardbolt-test)
