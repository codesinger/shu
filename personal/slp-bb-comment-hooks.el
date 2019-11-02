
;;
;; slp-comment-hooks.el
;;
;; Hooks that defie comment blocks in generated code
;;
;; Author: Stewart Palmer (spalmer62@bloomberg.net)


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
      "#ifndef INCLUDED_BSLALG_TYPETRAITS\n"
      "#include <bslalg_typetraits.h>\n"
      "#endif\n"
      "\n"
      "#ifndef INCLUDED_BSL_IOSTREAM\n"
      "#include <bsl_iostream.h>\n"
      "#endif\n"
      "\n"
      "#ifndef INCLUDED_BSLMA_ALLOCATOR\n"
      "#include <bslma_allocator.h>\n"
      "#endif\n"
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
      "// ----------------------------------------------------------------------------\n"
      "// Copyright (C) " (format-time-string "%Y") " Bloomberg L.P.\n"
      "//\n"
      "// Permission is hereby granted, free of charge, to any person obtaining a copy\n"
      "// of this software and associated documentation files (the \"Software\"), to\n"
      "// deal in the Software without restriction, including without limitation the\n"
      "// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or\n"
      "// sell copies of the Software, and to permit persons to whom the Software is\n"
      "// furnished to do so, subject to the following conditions:\n"
      "//\n"
      "// The above copyright notice and this permission notice shall be included in\n"
      "// all copies or substantial portions of the Software.\n"
      "//\n"
      "// THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n"
      "// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n"
      "// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE\n"
      "// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n"
      "// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING\n"
      "// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS\n"
      "// IN THE SOFTWARE.\n"
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
