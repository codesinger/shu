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



(defconst shu-cpp-attributes-nullable   1
  "Value that indicates that an attribute is nullable")


(defconst shu-cpp-attributes-reference  2
  "Value that indicates that an attribute is accessed by reference")


;;
;;
;;  shu-cpp-extract-attr-info
;;
(defmacro shu-cpp-extract-attr-info (attr-info name data-type full-data-type comment reference nullable)
  "Extract the information out of an attr-info"
  (let (
        (tattr-ext (make-symbol "attr-ext"))
        (tattr-other (make-symbol "attr-other"))
        (tattr-cmt (make-symbol "attr-cmt"))
        (tflags (make-symbol "flags"))
        )
    `(let (
           (,tattr-ext)
           (,tattr-other)
           (,tattr-cmt)
           (,tflags)
           )
       (setq ,name (car ,attr-info))
       (setq ,tattr-ext (cdr ,attr-info))
       (setq ,full-data-type (car ,tattr-ext))
       (setq ,tattr-other (cdr ,tattr-ext))
       (setq ,data-type (car ,tattr-other))
       (setq ,tattr-cmt (cdr ,tattr-other))
       (setq ,comment (car ,tattr-cmt))
       (setq ,tflags (cdr ,tattr-cmt))
       (if (= (logand ,tflags shu-cpp-attributes-reference) shu-cpp-attributes-reference)
           (setq ,reference t)
         (setq ,reference nil)
         )
       (if (= (logand ,tflags shu-cpp-attributes-nullable) shu-cpp-attributes-nullable)
           (setq ,nullable t)
         (setq ,nullable nil)
         )
       )
    ))



;;
;;  shu-cpp-extract-attr-info-name
;;
(defsubst shu-cpp-extract-attr-info-name (attr-info)
  "Return the name from the ATTR-INFO"
  (car attr-info)
  )


;;
;;  shu-cpp-make-attr-info
;;
(defun shu-cpp-make-attr-info (name data-type full-data-type &optional comment reference nullable)
  "Return an attr-info created from the given arguments"
  (let (
        (flags 0)
        )
    (when reference
      (setq flags shu-cpp-attributes-reference)
      )
    (when nullable
      (setq flags (logior flags shu-cpp-attributes-nullable))
      )
  (cons name
        (cons full-data-type
              (cons data-type
                    (cons comment flags))))
  ))



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
        (reference)
        (nullable)
        )
    (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment reference nullable)
    (princ (concat "name: [" name "], type: [" data-type "], ref: " (shu-bool-to-string nullable)
                   "], nullable: " (shu-bool-to-string nullable)
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
;;  shu-cpp-attributes-name-compare
;;
(defun shu-cpp-attributes-name-compare (lhs-attr-info rhs-attr-info)
  "Compare the names from LHS-ATTR-INFO and RHS-ATTR-INFO.  Return true if the left hand
name is less than the right hand name."
  (let (
        (lhs-name (shu-cpp-extract-attr-info-name lhs-attr-info))
        (rhs-name (shu-cpp-extract-attr-info-name rhs-attr-info))
        )
    (string< lhs-name rhs-name)
    ))




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
    (shu-cpp-attributes-gen-getter-has-decl attributes)
    (shu-cpp-attributes-gen-getter-decl attributes)
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
        (reference)
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
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment reference nullable)
      (when (> (length full-data-type) max-type-len)
        (setq max-type-len (length full-data-type))
        )
      (setq attrs (cdr attrs))
      )
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment reference nullable)
      (insert "\n")
      (when comment
        (insert (concat ipad "//! " comment " (" (number-to-string attr-num) ")\n"))
        (setq pad-count 0)
        (when (< (length full-data-type) max-type-len)
          (setq pad-count (- max-type-len (length full-data-type)))
          )
        )
      (setq pad-count (+ pad-count 3))
      (setq pad (make-string pad-count ? ))
      (insert (concat ipad full-data-type pad member-prefix name ";\n"))
      (setq attr-num (1+ attr-num))
      (setq attrs (cdr attrs))
      )
    ))



;;
;;  shu-cpp-attributes-gen-getter-has-decl
;;
(defun shu-cpp-attributes-gen-getter-has-decl (attributes)
  "Doc string."
  (let (
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (setq attrs (sort attrs 'shu-cpp-attributes-name-compare))
    (insert (concat "\n\n" ipad "// ACCESSORS\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment reference nullable)
      (when nullable
        (insert "\n")
        (when comment
          (insert
           (concat "\n"
                   ipad "/*!\n"
                   ipad " * Return true if " name " exists\n"
                   ipad " */\n"))
          )
        (setq uname (capitalize name))
        (insert (concat ipad "bool has" uname "() const;"))
        )
      (setq attrs (cdr attrs))
      )
    ))



;;
;;  shu-cpp-attributes-gen-getter-decl
;;
(defun shu-cpp-attributes-gen-getter-decl (attributes)
  "Doc string."
  (let (
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (setq attrs (sort attrs 'shu-cpp-attributes-name-compare))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment reference nullable)

        (insert "\n")
        (when comment
          (insert
           (concat "\n"
                   ipad "/*!\n"
                   ipad " * Return " comment "\n"
                   ipad " */\n"))
          )
        (setq uname (capitalize name))
        (insert (concat ipad data-type " " name"() const;"))

      (setq attrs (cdr attrs))
      )
    ))


;;    /*!
;;     * \brief Return a const reference to the deletion time
;;     */

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
        (reference t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment reference nullable))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xreference xnullable)
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

    (should xreference)

    (should xnullable)
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-2
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-2()
  (let (
        (name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference nil)
        (nullable t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment reference nullable))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xreference xnullable)

    (should (not xreference))

    (should xnullable)
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-3
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-3()
  (let (
        (name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference t)
        (nullable nil)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment reference nullable))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xreference xnullable)

    (should xreference)

    (should (not xnullable))
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-4
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-4()
  (let (
        (name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference nil)
        (nullable nil)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment reference nullable))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xreference xnullable)

    (should (not xreference))

    (should (not xnullable))
    ))




;;
;;  shu-test-shu-cpp-make-attr-info-5
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-5 ()
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
        (xreference)
        (xnullable)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xreference xnullable)
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

    (should (not xreference))
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-name
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-name ()
  (let (
        (name "frederick")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (nullable t)
        (attr-info)
        (xname)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment nullable))
    (setq xname (shu-cpp-extract-attr-info-name attr-info))
    (should xname)
    (should (stringp xname))
    (should (string= name xname))
    ))



;;; shu-attributes.el ends here
