;;; comment-hooks.el --- Shu project ...
;;
;; Copyright (C) 2018 Stewart L. Palmer
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
;;
;; comment-hooks.el
;;
;; Hooks that define comment blocks in generated code
;;


(defun usr-gen-file-id ()
  (insert "\n"))



(defun usr-gen-h-includes ()
  "Insert the standard includes that should be present in an empty,
generated header file."
  (insert
   (concat
      "\n"
      "#include <iostream>\n"
      ))
  )


(defun usr-gen-hcfile-copyright ()
  "Insert the standard copyright notice that should go into a generated header
file or code file."
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


(defun usr-gen-tfile-copyright ()
  "Insert the standard copyright notice that should go into a generated unit
test file."
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

(defun usr-set-comment-hooks()
  "Run this from your .emacs file to set the hooks for generated code."
  (interactive)
  (add-hook 'shu-bde-gen-file-identifier-hook 'usr-gen-file-id)
  (add-hook 'shu-bde-gen-h-includes-hook 'usr-gen-h-includes)
  (add-hook 'shu-bde-gen-hfile-copyright-hook 'usr-gen-hcfile-copyright)
  (add-hook 'shu-bde-gen-cfile-copyright-hook 'usr-gen-hcfile-copyright)
  (add-hook 'shu-bde-gen-tfile-copyright-hook 'usr-gen-tfile-copyright)
  )
