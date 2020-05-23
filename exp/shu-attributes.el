;;; shu-attributes.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2020 Stewart L. Palmer
;;
;; Package: shu-cpp-mch-funs
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


;;Things to copy per attribute
;;
;;   - name
;;   - data type
;;   - full data type
;;   - nullable
;;   - comment
;;
;;
;;
;;
;;  attr-info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-ext
;;        |
;;        +-------------> name
;;
;;
;;
;;
;;  attr-ext
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-other
;;        |
;;        +-------------> full-data-type
;;
;;
;;
;;
;;
;;  attr-other
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-cmt
;;        |
;;        +-------------> data-type
;;
;;
;;
;;
;;
;;  attr-cmt
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> nullable
;;        |
;;        +-------------> comment
;;
;;
;;
;;
;;
;;
;;  shu-cpp-extract-attr-info
;;
(defmacro shu-cpp-extract-attr-info (attr-info name data-type full-data-type comment nullable)
  "Extract the information out of an attr-info"
  (let (
        (tattr-ext (make-symbol "attr-ext"))
        (tattr-other (make-symbol "attr-other"))
        (tattr-cmt (make-symbol "attr-cmt"))
        )
    `(let (
           (,tattr-ext)
           (,tattr-other)
           (,tattr-cmt)
           )
       (setq ,name (car ,attr-info))
       (setq ,tattr-ext (cdr ,attr-info))
       (setq ,full-data-type (car ,tattr-ext))
       (setq ,tattr-other (cdr ,tattr-ext))
       (setq ,data-type (car ,tattr-other))
       (setq ,tattr-cmt (cdr ,tattr-other))
       (setq ,comment (car ,tattr-cmt))
       (setq ,nullable (cdr ,tattr-cmt))
       )
    ))



;;
;;  shu-cpp-make-attr-info
;;
(defun shu-cpp-make-attr-info (name data-type full-data-type &optional comment nullable)
  "Return an attr-info created from the given arguments"
  (setq debug-on-error t)
  (cons name
        (cons full-data-type
              (cons data-type
                    (cons comment nullable))))
  )



;;
;;  shu-cpp-print-attr-info
;;
(defun shu-cpp-print-attr-info (attr-info buf)
  "Print the contents of ATTR-INFO into the buffer BUF"
  (interactive)
  (let (
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (nullable)
        )
    (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment nullable)
    (princ (concat "name: [" name "], type: [" data-type "], nullable: " (shu-bool-to-string nullable)
                   ", full-type: [" full-data-type "]\n") buf)
    (when comment
      (princ (concat "    [" comment "]\n") buf)
      )


    ))





;;
;;  shu-cpp-make-nullable
;;
(defun shu-cpp-make-nullable (data-type)
  "Render a data type nullable"
  (concat "bdlb::NullableValue<" data-type ">")
  )





;;
;;  zzx
;;
(defun zzx (start end)
  "Doc string."
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (sline (shu-the-line-at start))
        (eline (shu-the-line-at end))
        (line-diff 0)
        (eol)
        (line)
        (x)
        (data-type)
        (full-data-type)
        (name)
        (comment)
        (nullable)
        (attr-info)
        (attributes)
        )
    (while (and (<= (shu-current-line) eline) (= line-diff 0)) ; there are more lines
      (setq eol (line-end-position))
      (when (> eol (point))
        (setq line (shu-trim (buffer-substring-no-properties (point) eol)))
        (princ (concat "\n\nline: [" line "]\n") gb)
        (when (> (length line) 1)
          (if (string= (substring line 0 2) "//")
              (progn
                (setq comment (shu-trim (substring line 2)))
                (princ (concat "comment: [" comment "]\n") gb)
                )
            (setq x (split-string line nil t))
            (setq data-type (car x))
            (setq x (cdr x))
            (setq name (car x))
            (setq nullable nil)
            (setq full-data-type data-type)
            (when (cdr x)
              (setq nullable t)
              (setq full-data-type (shu-cpp-make-nullable data-type))
              )
            )
          (when name
            (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment nullable))
            (push attr-info attributes)
            (shu-cpp-print-attr-info attr-info gb)
          (setq comment nil)
          (setq name nil)
            )
          )
        )
      (setq line-diff (forward-line 1))
      )
    (setq line-diff (forward-line 1))
    (setq attributes (nreverse attributes))
    (shu-cpp-attributes-gen attributes)
    ))


;;
;;  shu-cpp-attributes-gen
;;
(defun shu-cpp-attributes-gen (attributes)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        )
    (princ  "\n\nAttributes:\n" gb)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-print-attr-info attr-info gb)
      (setq attrs (cdr attrs))
      )
    (goto-char (point-max))
    (insert "\n\n")
    (shu-cpp-attributes-gen-decl attributes)
    ))



;;
;;  shu-cpp-attributes-gen-decl
;;
(defun shu-cpp-attributes-gen-decl (attributes)
  "Doc string."
  (let (
        (attrs attributes)
        (attr-info)
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (nullable)
        (max-type-len 31)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (insert (concat "\n\n" ipad "// DATA\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment nullable)
      (when (> (length full-data-type) max-type-len)
        (setq max-type-len (length full-data-type))
        )
      (setq attrs (cdr attrs))
      )
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment nullable)
      (insert "\n")
      (when comment)
      (insert (concat ipad "//! " comment " (" (number-to-string attr-num) ")\n"))
      (setq pad-count 0)
      (when (< (length full-data-type) max-type-len)
        (setq pad-count (- max-type-len (length full-data-type)))
        )
      (setq pad-count (+ pad-count 3))
      (setq pad (make-string pad-count ? ))
      (insert (concat ipad full-data-type pad member-prefix name ";\n"))
      (setq attr-num (1+ attr-num))
      (setq attrs (cdr attrs))
      )



    ))

;;
;;    bdlt::Datetime                    m_deleteTime;
;;    1234567890123456789012345678901



;;
;;  shu-test-shu-cpp-make-attr-info-1
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-1 ()
  (let (
        (name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (nullable t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment nullable))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xnullable)
    (should xname)
    (should (stringp xname))
    (should (string= name xname))

    (should xdata-type)
    (should (stringp xdata-type))
    (should (string= data-type xdata-type))

    (should xfull-data-type)
    (should (stringp xfull-data-type))
    (should (string= full-data-type xfull-data-type))

    (should xcomment)
    (should (stringp xcomment))
    (should (string= comment xcomment))

    (should xnullable)
    ))




;;
;;  shu-test-shu-cpp-make-attr-info-2
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-2 ()
  (let (
        (name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (nullable t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xnullable)
    (should xname)
    (should (stringp xname))
    (should (string= name xname))

    (should xdata-type)
    (should (stringp xdata-type))
    (should (string= data-type xdata-type))

    (should xfull-data-type)
    (should (stringp xfull-data-type))
    (should (string= full-data-type xfull-data-type))

    (should (not xcomment))

    (should (not xnullable))
    ))



;;; shu-attributes.el ends here
