;;; beardbolt-split-test.el --- Tests for beardbolt-split -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for beardbolt

;;; Code:

(require 'beardbolt-split)

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

(provide 'beardbolt-split-test)

;;; beardbolt-split-test.el ends here
