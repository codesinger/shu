;;; shu-exp-dpkg.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2020 Stewart L. Palmer
;;
;; Package: shu-exp-dpkg
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; There is a copy of the Gnu General Public license in the file
;; LICENSE in this repository.  You should also have received a copy
;; of the GNU General Public License along with GNU Emacs.  If not,
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A collection of experimental functions for dealing with C++ code.
;;
;;

;;; Code:

(provide 'shu-exp-dpkg)
(require 'shu-base)
(require 'shu-misc)


;;
;;  shu-gen-dpkg
;;
(defun shu-gen-dpkg (library-name)
  "Generate an empty DPKG library."
  (interactive "sLibrary name?: ")
  (let (
        (git-namespace "drqs1011")
        (dpkg-author "Stewart Palmer <spalmer62@bloomberg.net>")
        (debug-on-error t)
        )
    (shu-internal-gen-dpkg library-name dpkg-author git-namespace)
    ))


;;
;;  shu-internal-gen-dpkg
;;
(defun shu-internal-gen-dpkg (library-name author git-namespace)
  (let* ((log-buffer-name "**shu-dpkg**")
         (gb (get-buffer-create log-buffer-name))
         (base-name default-directory)
         (top-cmake-name (concat base-name "CMakeLists.txt"))
         (debian-name (concat base-name "debian"))
         (jenkins-name (concat base-name "Jenkinsfile"))
         (control-name (concat debian-name "/control"))
         (rules-name (concat debian-name "/rules"))
         (package-directory (concat base-name library-name "/package"))
         (test-directory (concat base-name library-name "/t"))
         (package-dep (concat package-directory "/" library-name ".dep"))
         (package-mem (concat package-directory "/" library-name ".mem"))
         (mainpage-name (concat base-name library-name "/mainpage.dox"))
         (main-cmake-name (concat base-name library-name "/CMakeLists.txt"))
         (main-make-name (concat base-name library-name "/" library-name ".mk"))
         (make1-name (concat test-directory "/" library-name ".t.mk"))
         (make2-name (concat test-directory "/" library-name ".build.t.mk"))
         (test-cmake-name (concat test-directory "/CMakeLists.txt"))
         (error-string))
    (unless (file-directory-p library-name)
      (make-directory package-directory t)
      (make-directory test-directory t)
      (shu-internal-make-mainpage library-name mainpage-name)
      (find-file package-mem)
      (goto-char (point-min))
      (insert
       (concat
       "# [DEPENDENCY BUILD]\n"
       "# [PREBUILT LEGACY]\n"
       "# [OFFLINE ONLY]\n"
        ))
      (basic-save-buffer)
      (kill-buffer (current-buffer))
      (find-file package-dep)
      (goto-char (point-min))
      (insert
       "bal\n"
       "bdl\n"
       "bsl"
       (concat
        ))
      (basic-save-buffer)
      (kill-buffer (current-buffer)))
    (shu-dpkg-internal-main-make main-make-name library-name)
    (shu-dpkg-internal-gen-make make1-name make2-name library-name)
    (unless (file-readable-p jenkins-name)
      (find-file jenkins-name)
      (goto-char (point-min))
      (insert
       (concat
        "@Library('fxbuild-pipeline') _\n"
        "fxbuild_pipeline(['fxbuild_package': '" library-name "'], scm)"
        ))
      (basic-save-buffer)
      (kill-buffer (current-buffer)))
    (unless (file-directory-p debian-name)
      (make-directory debian-name)
      (shu-internal-gen-debian-control library-name control-name author git-namespace)
      (shu-internal-gen-debian-rules library-name rules-name))
    (shu-dpkg-make-top-level-cmake library-name top-cmake-name)
    (shu-dpkg-make-main-cmake library-name main-cmake-name)
    (shu-dpkg-make-test-cmake library-name test-cmake-name)
    (setq error-string (shu-dpkg-populate-git-add library-name))
    (when error-string
      (princ error-string gb)
      (message "git add failures: See buffer %s" log-buffer-name))
    ))


;;
;;  shu-dpkg-populate-git-add
;;
(defun shu-dpkg-populate-git-add (library-name)
  "LIBRARY-NAME is the name of the library.  This function issues `git add`
requests for the top level items in the newly created DPKG library.  Any error
messages from any of the `git add` commands are returned in a single string.  If
the returned string is nil, all of the `git add` commands worked."
  (let ((error-string)
        (names
         (list
          "CMakeLists.txt"
          "Jenkinsfile"
          "debian"
          library-name
          ))
        (nn)
        (name)
        (result))
    (setq nn names)
    (while nn
      (setq name (car nn))
      (setq result (shu-git-add-file name))
      (when (not (string= result ""))
        (setq error-string (concat error-string result "\n")))
      (setq nn (cdr nn)))
    error-string
    ))



;;
;;  shu-internal-gen-debian-control
;;
(defun shu-internal-gen-debian-control (library-name control-name author git-namespace)
  "Doc string."
  (let ((gb (get-buffer-create "**shu-dpkg**")))
    (princ (concat "control: " default-directory "\n") gb)
    (unless (file-readable-p control-name)
      (princ (concat "control2: " default-directory "\n") gb)
      (find-file control-name)
      (princ (concat "control3: " default-directory "\n") gb)
      (goto-char (point-min))
      (insert
       (concat
       "Source: " library-name "\n"
       "Section: unknown\n"
       "Priority: extra\n"
       "Build-Depends:\n"
       "  bbtoolchain-cmake-debhelper,\n"
       "  cmake-configure-bb-target,\n"
       "  libbal-dev,\n"
       "  libbdl-dev,\n"
       "  libbsl-dev,\n"
       "  libcmdtst-dev,\n"
       "  libcmdtstmain-dev,\n"
       "  libgmock-robo-dev,\n"
       "  libgtest-robo-dev\n"
       "Maintainer: " author "\n"
       "Vcs-Git: git@bbgithub.dev.bloomberg.com:" git-namespace "/" library-name "\n"
       "Standards-Version: 3.8.4\n"
       "\n"
       "Package: lib" library-name "-dev\n"
       "Depends: ${" library-name "-bdemeta-Depends}\n"
       "Architecture: any\n"
       "Description: FIXME FIXME FIXME FIXME FIXME FIXME FIXME!!!"
        ))
      (basic-save-buffer)
      (kill-buffer (current-buffer)))
    ))


;;
;;  shu-internal-gen-debian-rules
;;
(defun shu-internal-gen-debian-rules (library-name rules-name)
  "Doc string."
  (let ((gb (get-buffer-create "**shu-dpkg**")))
    (princ (concat "rules: " default-directory "\n") gb)
    (princ (concat "rules-name : " rules-name "\n") gb)
    (unless (file-readable-p rules-name)
      (princ (concat "rules2: " default-directory "\n") gb)
      (find-file rules-name)
      (princ (concat "rules3: " default-directory "\n") gb)
      (goto-char (point-min))
      (insert
       (concat
        "#!/usr/bin/make -f\n"
        "# -*- makefile -*-\n"
        "DEBHELPER_PATH=$(DISTRIBUTION_REFROOT)/opt/bb/share/plink\n"
        "include $(DEBHELPER_PATH)/plink-debhelper-macros.mk\n"
        "\n"
        "$(eval $(call LIBRARY_PACKAGE_TEMPLATE," library-name "))\n"
        "$(eval $(call RUNTESTS_TEMPLATE," library-name "))\n"
        "\n"
        "include $(DEBHELPER_PATH)/plink-debhelper-rules.mk\n"
        ))
      (basic-save-buffer)
      (kill-buffer (current-buffer)))
    ))


;;
;;  shu-internal-make-mainpage
;;
(defun shu-internal-make-mainpage (library-name mainpage-name)
  "Doc string."
  (interactive)
  (let ((outer-close "}  // close enterprise namespace\n")
        (outer-namespace))
    (if shu-cpp-default-global-namespace
        (setq outer-namespace (concat "namespace " shu-cpp-default-global-namespace " {\n"))
      (setq outer-close ""))
    (find-file mainpage-name)
      (goto-char (point-min))
      (insert
       (concat
        "/*!\n"
        " * \\file mainpage.dox\n"
        " *\n"
        " * \\brief Stand alone Doxygen documentation\n"
        " *\n"
        " */\n"
        "\n"
        outer-namespace
        "namespace " library-name " {\n"
        "\n"
        "\n"
        "\n"
        "/*!\n"
        "\n"
        "\n"
        "\n"
        "\\mainpage " library-name " INSERT YOUR ONE LINE SUMMARY HERE\n"
        "\n"
        "\n"
        "## section\n"
        "\n"
        "Doxygen supports markdown within Doxygen comments.\n"
        "\n"
        "\n"
        "\n"
        "*/\n"
        "\n"
        "\n"
        "\n"
        "}  // close package namespace\n"
        outer-close
        "\n"
        "\n"
      ))
    (insert
     (concat
      (run-hooks 'shu-bde-gen-cfile-copyright-hook)
      "// ----------------------------- END-OF-FILE ---------------------------------\n"
      ))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))




;;
;;  shu-dpkg-internal-gen-make
;;
(defun shu-dpkg-internal-gen-make (make1-name make2-name library-name)
  "Doc string."
  (interactive)
    (find-file make1-name)
      (goto-char (point-min))
      (insert
       (concat
        "TESTS=" library-name ".build.t\n"
        "TESTS_tsk=$(patsubst %,%.tsk,$(TESTS))\n"
        "\n"
        "check: $(TESTS_tsk)\n"
        "		echo \"Ran unit tests for $(TESTS_tsk)\"\n"
        "\n"
        "%.tsk: %.mk must-always-run-this-target\n"
        "	$(MAKE) -f $< $@\n"
        "\n"
        ".PHONY: must-always-run-this-target check"
       ))
      (basic-save-buffer)
      (kill-buffer (current-buffer))
    (find-file make2-name)
      (goto-char (point-min))
      (insert
       (concat
        "PCDEPS += cmdtst\n"
        "PCDEPS += cmdtstmain\n"
        "PCDEPS += bal\n"
        "PCDEPS += bdl\n"
        "PCDEPS += bsl\n"
        "TASK=" library-name ".build.t.tsk\n"
        "SRCS= \\\n"
        "../" library-name "_something.cpp \\\n"
        library-name "_something.t.cpp\n"
        "\n"
        "USER_CFLAGS   += -I. -I..\n"
        "USER_CPPFLAGS += -I. -I..\n"
        "USER_FFLAGS   += -I. -I..\n"
        "\n"
        "IS_GTEST=yes\n"
        "IS_GCC_WARNINGS_CLEAN=yes\n"
        "\n"
        "METAMKMK_VER=1.0\n"
        "IS_CPPMAIN=1\n"
        "IS_DEPENDS_NATIVE=1\n"
        "MKINCL?=/bbsrc/mkincludes/\n"
        "include $(MKINCL)sourcelist.mk\n"
        "include $(LIBMACROS_MK)\n"
        "OBJS?=$(OBJS_AR)\n"
        "include $(MKINCL)machdep.newlink\n"
        "include $(MKINCL)linktask.newlink\n"
        "\n"
        "" library-name ".build.t.tsk: " library-name ".build.t.$(ARCHCODE)$(ABI).tsk\n"
        "	echo \"IN BUILD\"\n"
        "	rm -f $@\n"
        "	ln $< $@\n"
        "	./$@\n"
        "\n"
        "\n"
        "\n"
        ".PHONY: test\n"
        "test:\n"
        "	EXTSHM=ON ./$(ARCHTASK) \\\n"
        "		--bael-format \"%d %p:%t %s %f:%l %c %m %u\" \\\n"
        "		--gtest_filter='*.*' \\\n"
        "		--bael-log-on-success \\\n"
        "		--bael-level INFO \\\n"
        "		--gtest_output=xml:test_detail.xml\n"
       ))
      (basic-save-buffer)
      (kill-buffer (current-buffer))
    )




;;
;;  shu-dpkg-internal-main-make
;;
(defun shu-dpkg-internal-main-make (main-make-name library-name)
  "Doc string."
  (interactive)
    (find-file main-make-name)
      (goto-char (point-min))
      (insert
       (concat
        "# -*- makefile -*-\n"
        "# $Id$ $CSID$\n"
        "\n"
        "LIBNAME=" library-name "\n"
        "\n"
        "USER_CPPFLAGS += -I.\n"
        "\n"
        "IS_GCC_WARNINGS_CLEAN=yes\n"
        "IS_OPTIMIZED=yes\n"
        "IS_BDE=yes\n"
        "IS_PTHREAD=yes\n"
        "IS_EXCEPTION=yes\n"
        "IS_DEPENDS_NATIVE=1\n"
        "\n"
        "MKINCL?=/bbsrc/mkincludes/\n"
        "include $(MKINCL)machindepy.lib\n"
        "\n"
        "# vim:ft=make\n"
       ))
      (basic-save-buffer)
      (kill-buffer (current-buffer))
      )



;;
;;  shu-dpkg-make-top-level-cmake
;;
(defun shu-dpkg-make-top-level-cmake (library-name cmake-name)
  (let ((gb (get-buffer-create "**shu-dpkg**")))
    (princ (concat "make-top-level-cmake: '" cmake-name "'\n") gb)
    (find-file cmake-name)
    (goto-char (point-min))
    (insert
     (concat
      "cmake_minimum_required(VERSION 3.14)\n"
      "project(" library-name " LANGUAGES C CXX)\n"
      "\n"
      "enable_testing()\n"
      "\n"
      "SET( " library-name "_BUILD_LOCAL TRUE )\n"
      "SET( test_t.tsk_BUILD_LOCAL TRUE )\n"
      "\n"
      "add_subdirectory(" library-name" )\n"
      "\n"
      "#==============================================================================\n"
      "# Fix Linking of cyclic libraries\n"
      "# http://tutti.prod.bloomberg.com/cmake-docs/troubleshooting/troubleshooting#unresolved-symbols\n"
      "# Uncomment required libraries below\n"
      "#==============================================================================\n"
      "#set_property( TARGET apputil PROPERTY IMPORTED_LINK_INTERFACE_MULTIPLICITY 5 )\n"
      ))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))



;;
;;  shu-dpkg-make-main-cmake
;;
(defun shu-dpkg-make-main-cmake (library-name cmake-name)
  (let ((gb (get-buffer-create "**shu-dpkg**")))
    (princ (concat "make-main-cmake: '" cmake-name "'\n") gb)
    (find-file cmake-name)
    (goto-char (point-min))
    (insert
     (concat
      "add_subdirectory(t)\n"
      "#==============================================================================\n"
      "# Add Library\n"
      "# https://cmake.org/cmake/help/v3.14/command/add_library.html\n"
      "#==============================================================================\n"
      "add_library(" library-name" \n"
      "        " library-name "_something.cpp\n"
      "        )\n"
      "\n"
      "#==============================================================================\n"
      "# Add Link Dependencies\n"
      "# https://cmake.org/cmake/help/v3.14/command/target_link_libraries.html\n"
      "#==============================================================================\n"
      "target_link_libraries(" library-name"  PUBLIC\n"
      "        # external dependencies\n"
      "        bdl\n"
      "        bsl\n"
      "        )\n"
      "\n"
      "#==============================================================================\n"
      "# Configure Target for use at Bloomberg\n"
      "# https://bbgithub.dev.bloomberg.com/cmake-community/cmake-configure-bb-target\n"
      "#==============================================================================\n"
      "find_package(ConfigureBbTarget REQUIRED)\n"
      "configure_bb_target(" library-name " V2 INSTALL HEADER_DIRS .)\n"
      ))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))



;;
;;  shu-dpkg-make-test-cmake
;;
(defun shu-dpkg-make-test-cmake (library-name cmake-name)
  (let ((gb (get-buffer-create "**shu-dpkg**")))
    (princ (concat "make-test-cmake: '" cmake-name "'\n") gb)
    (find-file cmake-name)
    (goto-char (point-min))
    (insert
     (concat
      "#==============================================================================\n"
      "# Add Executable\n"
      "# https://cmake.org/cmake/help/v3.14/command/add_executable.html\n"
      "#==============================================================================\n"
      "add_executable(test_t.tsk\n"
      "        " library-name " _something.t.cpp\n"
      "        )\n"
      "\n"
      "#==============================================================================\n"
      "# Add Link Dependencies\n"
      "# https://cmake.org/cmake/help/v3.14/command/target_link_libraries.html\n"
      "#==============================================================================\n"
      "target_link_libraries(test_t.tsk PUBLIC\n"
      "        cmdtstmain\n"
      "        # local dependencies\n"
      "        " library-name "\n"
      "        # external dependencies\n"
      "        bal\n"
      "        bdl\n"
      "        bsl\n"
      "        )\n"
      "\n"
      "#==============================================================================\n"
      "# Configure Target for use at Bloomberg\n"
      "# https://bbgithub.dev.bloomberg.com/cmake-community/cmake-configure-bb-target\n"
      "#==============================================================================\n"
      "find_package(ConfigureBbTarget REQUIRED)\n"
      "configure_bb_target(test_t.tsk V2 HEADER_DIRS .)\n"
      "\n"
      "#==============================================================================\n"
      "# Add Test\n"
      "# https://cmake.org/cmake/help/v3.14/module/GoogleTest.html\n"
      "#==============================================================================\n"
      "include(GoogleTest)\n"
      "gtest_discover_tests(test_t.tsk)\n"
      ))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))



;;; shu-exp-dpkg.el ends here
