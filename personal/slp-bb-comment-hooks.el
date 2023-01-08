;;; slp-bb-comment-hooks.el --- Personal comment hooks for use with the Shu project
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


(defun slp-bb-gen-file-id ()
  (insert
   (concat
      "#ifndef INCLUDED_BSLS_IDENT\n"
      "#include <bsls_ident.h>\n"
      "#endif\n"
      "BSLS_IDENT(\"$Id: $\")\n")
   ))



(defun slp-bb-gen-h-includes ()
  (insert
   (concat
      "\n"
      "#include <bsl_ostream.h>\n"
      "#include <bslma_allocator.h>\n"
      "#include <bslma_usesbslmaallocator.h>\n"
      "#include <bslmf_nestedtraitdeclaration.h>\n"
      ))
  )


(defun slp-bb-gen-hcfile-copyright ()
  (insert
   (concat
      "// ---------------------------------------------------------------------------\n"
      "// NOTICE:\n"
      "//      Copyright (C) Bloomberg L.P., " (format-time-string "%Y") "\n"
      "//      All Rights Reserved.\n"
      "//      Property of Bloomberg L.P. (BLP)\n"
      "//      This software is made available solely pursuant to the\n"
      "//      terms of a BLP license agreement which governs its use.\n"
      ))
  )


(defun slp-bb-gen-tfile-copyright ()
  (insert
   (concat
      "// ---------------------------------------------------------------------------\n"
      "// NOTICE:\n"
      "//      Copyright (C) Bloomberg L.P., " (format-time-string "%Y") "\n"
      "//      All Rights Reserved.\n"
      "//      Property of Bloomberg L.P. (BLP)\n"
      "//      This software is made available solely pursuant to the\n"
      "//      terms of a BLP license agreement which governs its use.\n"
      ))
  )

(defun slp-bb-set-comment-hooks()
  (interactive)
  (add-hook 'shu-bde-gen-file-identifier-hook 'slp-bb-gen-file-id)
  (add-hook 'shu-bde-gen-h-includes-hook 'slp-bb-gen-h-includes)
  (add-hook 'shu-bde-gen-hfile-copyright-hook 'slp-bb-gen-hcfile-copyright)
  (add-hook 'shu-bde-gen-cfile-copyright-hook 'slp-bb-gen-hcfile-copyright)
  (add-hook 'shu-bde-gen-tfile-copyright-hook 'slp-bb-gen-tfile-copyright)
  )


;;; slp-bb-comment-hooks.el ends here
