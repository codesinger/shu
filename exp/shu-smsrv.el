;;; shu-smsrv.el --- Shu project code for dealing with encrypted keyrings
;;
;; Copyright (C) 2024 Stewart L. Palmer
;;
;; Package: shu-smsrv
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

;; This is an experimental elisp function that may, in the future,
;; be retained in some form

;;; Code:



;;
;;  shu-make-schema-rmv
;;
(defun shu-make-schema-rmv ()
  "Current buffer holds a list of namespace and class names from a BAS code
generated schema.h file of the form

    namespace xxxxx { class Mumble; }

Generated in the buffer **shu-lisp-output**, the emacs lisp code necessary to
run RMV-USING on any file that references these class names as unqualified
names.

This is used to migrate from a BAS service in which the schema class names are
in the same namespace as the service to a more recently generated BAS service in
which the shcme generated class names are in their own library namespace."
  (interactive)
  (let ((gb (get-buffer-create "**shu-lisp-output**"))
        (exp (concat "\\s-*namespace\\s-*\\(" shu-cpp-name "+\\)\\s-*{\\s-*class\\s-*\\(" shu-cpp-name "+\\);"))
        (namespace)
        (classname)
        (ns)
        (cnames)
        (nnames)
        (nns)
        (cns)
        (indent-length)
        (ispace))
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward exp nil t))
          (progn
            (ding)
            (message "%s" "Cannot find pattern namespace xxxx { class yyyy; }"))
        (setq namespace (match-string 1))
        (goto-char (point-min))
        (while (re-search-forward exp nil t)
          (setq ns (match-string 1))
          (setq classname (match-string 2))
          (push ns nnames)
          (push classname cnames))
        (setq nnames (nreverse nnames))
        (setq cnames (nreverse cnames))
        (setq nns nnames)
        (setq cns cnames)
        (while cns
          (setq classname (car cns))
          (setq ns (car nns))
          (when (not (string= ns namespace))
            (princ (format "WARNING: Class name %s is in namespace %s\n" classname ns) gb))
          (setq cns (cdr cns))
          (setq nns (cdr nns)))

        (setq indent-length (+ (length classname) 14))
        (setq ispace (make-string indent-length ? ))
        (princ (concat
                "(defconst local-class-list\n"
                "  (list\n"
                ) gb)
        (princ (format "   (cons \"%s\" (list\n" namespace) gb)
        (setq cns cnames)
        (while cns
          (setq classname (car cns))
          (princ (concat ispace "\"" classname "\"\n") gb)
          (setq cns (cdr cns)))
        (princ (concat
                ispace "))\n"
                "   ))\n"
                ) gb)
        (princ
         (concat
          "\n"
          "\n"
          "\n"
          ";;\n"
          ";;  rmv-using\n"
          ";;\n"
          "(defun rmv-using ()\n"
          "  \"Remove all using namespace directives from a file, replacing the\n"
          "unqualified class names with qualified class names.  See the documentation\n"
          "for the variable slp-fx-class-list for a list of namespaces and classes this\n"
          "function knows about.\"\n"
          "  (interactive)\n"
          "  (let ((top-name \"BloombergLP\")\n"
          "        )\n"
          "    (shu-cpp-rmv-using local-class-list top-name)\n"
          "    ))\n"
          ) gb)))
    ))


;;; shu-smsrv.el ends here
