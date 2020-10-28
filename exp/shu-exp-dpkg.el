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


;;
;;  shu-gen-dpkg
;;
(defun shu-gen-dpkg (library-name)
  "Doc string."
  (interactive "sLibrary name?: ")
  (let (
        (git-namespace "drqs68")
        (dpkg-author "Stewart Palmer <spalmer62@bloomberg.net>")
        )
    (shu-internal-gen-dpkg library-name dpkg-author git-namespace)

    ))



;;
;;  whatever
;;
(defun whatever ()
  "Doc string."
  (interactive)
  (let* (
         (gb (get-buffer-create "**boo**"))
         (library-name "fxwonderful")
         (base-name default-directory)
         (debian-name (concat base-name "debian"))
         (jenkins-name (concat base-name "Jenkinsfile"))
         (control-name (concat debian-name "/control"))
         (rules-name (concat debian-name "/rules"))
         (package-directory (concat base-name library-name "/package"))
         )
    (princ (concat "base-name: " base-name "\n") gb)
    (princ (concat "debian-name: " debian-name "\n") gb)
    (princ (concat "jenkins-name: " jenkins-name "\n") gb)
    (princ (concat "control-name: " control-name "\n") gb)
    (princ (concat "rules-name: " rules-name "\n") gb)
    (princ (concat "package-directory: " package-directory "\n") gb)

    ))


;;
;;  shu-internal-gen-dpkg
;;
(defun shu-internal-gen-dpkg (library-name author git-namespace)
  "Doc string."
  (interactive)
  (let* (
         (gb (get-buffer-create "**boo**"))
         (base-name default-directory)
         (debian-name (concat base-name "debian"))
         (jenkins-name (concat base-name "Jenkinsfile"))
         (control-name (concat debian-name "/control"))
         (rules-name (concat debian-name "/rules"))
         (package-directory (concat base-name library-name "/package"))
         (package-dep (concat package-directory "/" library-name ".dep"))
         (package-mem (concat package-directory "/" library-name ".mem"))
         (mainpage-name (concat base-name library-name "/mainpage.dox"))
        )
    (unless (file-directory-p library-name)
      (make-directory package-directory t)
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
      (kill-buffer (current-buffer))
      )
    (unless (file-readable-p jenkins-name)
      (find-file jenkins-name)
      (goto-char (point-min))
      (insert
       (concat
        "@Library('fxbuild-pipeline') _\n"
        "fxbuild_pipeline(['fxbuild_package': '" library-name " '], scm)"
        ))
      (save-buffer)
      )
    (unless (file-directory-p debian-name)
      (make-directory debian-name)
      (shu-internal-gen-debian-control library-name control-name author git-namespace)
      (shu-internal-gen-debian-rules library-name rules-name)
      )

    ))


;;
;;  shu-internal-gen-debian-control
;;
(defun shu-internal-gen-debian-control (library-name control-name author git-namespace)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        )
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
       "Build-Depends: plink, plink-debhelper,\n"
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
       "Description: FIXME FIXME FIXME DIXME!!!"
        ))
      (basic-save-buffer)
      (kill-buffer (current-buffer))
      )

    ))


;;
;;  shu-internal-gen-debian-rules
;;
(defun shu-internal-gen-debian-rules (library-name rules-name)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        )
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
      (kill-buffer (current-buffer))
      )

    ))


;;
;;  shu-internal-make-mainpage
;;
(defun shu-internal-make-mainpage (library-name mainpage-name)
  "Doc string."
  (interactive)
  (let (
        (outer-close "}  // close enterprise namespace\n")
        (outer-namespace)
        )
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


;;; shu-exp-dpkg.el ends here
