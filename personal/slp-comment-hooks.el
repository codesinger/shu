;;; slp-comment-hooks.el --- Peersonal comment hooks for use with the Shu project
;;
;; Copyright (C) 2016 Stewart L. Palmer
;;
;; Package: shu-base
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
;;

;;; Commentary:

;; Collection of comment hooks for use with shu functions that generate
;; C++ code.  This is part of my personal customization and is not part of
;; the shu elisp project.

;;; Code:



;;
;; slp-comment-hooks.el
;;
;; Hooks that define comment blocks in generated code
;;


(defun slp-gen-file-id ()
  (insert "\n"))



(defun slp-gen-h-includes ()
  (insert
   (concat
      "\n"
      "#include <ostream>\n"
      ))
  )


(defun slp-gen-hcfile-copyright ()
  (insert
   (concat
      "// ----------------------------------------------------------------------------\n"
      "// This program is free software: you can redistribute it and/or modify\n"
      "// it under the terms of the GNU General Public License as published by\n"
      "// the Free Software Foundation, either version 3 of the License, or\n"
      "// (at your option) any later version.\n"
      "// \n"
      "// This program is distributed in the hope that it will be useful,\n"
      "// but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
      "// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
      "// GNU General Public License for more details.\n"
      "// \n"
      "// You should have received a copy of the GNU General Public License\n"
      "// along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"
      ))
  )


(defun slp-gen-tfile-copyright ()
  (insert
   (concat
      "// ----------------------------------------------------------------------------\n"
      "// This program is free software: you can redistribute it and/or modify\n"
      "// it under the terms of the GNU General Public License as published by\n"
      "// the Free Software Foundation, either version 3 of the License, or\n"
      "// (at your option) any later version.\n"
      "// \n"
      "// This program is distributed in the hope that it will be useful,\n"
      "// but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
      "// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
      "// GNU General Public License for more details.\n"
      "// \n"
      "// You should have received a copy of the GNU General Public License\n"
      "// along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"      ))
  )

(defun slp-set-comment-hooks()
  (interactive)
  (add-hook 'shu-bde-gen-file-identifier-hook 'slp-gen-file-id)
  (add-hook 'shu-bde-gen-h-includes-hook 'slp-gen-h-includes)
  (add-hook 'shu-bde-gen-hfile-copyright-hook 'slp-gen-hcfile-copyright)
  (add-hook 'shu-bde-gen-cfile-copyright-hook 'slp-gen-hcfile-copyright)
  (add-hook 'shu-bde-gen-tfile-copyright-hook 'slp-gen-tfile-copyright)
  )


;;; slp-comment-hooks.el ends here
