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

(provide 'shu-attributes)
(require 'shu-base)


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
;;        |       +-----> attr-col
;;        |
;;        +-------------> comment
;;
;;
;;
;;
;;
;;  attr-col
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> column-name
;;        |
;;        +-------------> nullable
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
(defmacro shu-cpp-extract-attr-info (attr-info name data-type full-data-type
                                               comment reference nullable column-name)
  "Extract the information out of an attr-info"
  (let (
        (tattr-ext (make-symbol "attr-ext"))
        (tattr-other (make-symbol "attr-other"))
        (tattr-cmt (make-symbol "attr-cmt"))
        (tattr-col (make-symbol "attr-col"))
        (tflags (make-symbol "flags"))
        )
    `(let (
           (,tattr-ext)
           (,tattr-other)
           (,tattr-cmt)
           (,tattr-col)
           (,tflags)
           )
       (setq ,name (car ,attr-info))
       (setq ,tattr-ext (cdr ,attr-info))
       (setq ,full-data-type (car ,tattr-ext))
       (setq ,tattr-other (cdr ,tattr-ext))
       (setq ,data-type (car ,tattr-other))
       (setq ,tattr-cmt (cdr ,tattr-other))
       (setq ,tattr-col (cdr ,tattr-cmt))
       (setq ,comment (car ,tattr-cmt))
       (setq ,tflags (car ,tattr-col))
       (if (= (logand ,tflags shu-cpp-attributes-reference) shu-cpp-attributes-reference)
           (setq ,reference t)
         (setq ,reference nil)
         )
       (if (= (logand ,tflags shu-cpp-attributes-nullable) shu-cpp-attributes-nullable)
           (setq ,nullable t)
         (setq ,nullable nil)
         )
       (setq ,column-name (cdr ,tattr-col))
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
(defun shu-cpp-make-attr-info (name data-type full-data-type &optional comment
                                    reference nullable column-name)
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
                      (cons comment
                            (cons flags column-name)))))
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
        (column-name)
        )
    (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment reference nullable column-name)
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
;;  shu-upcase-first-letter
;;
(defun shu-upcase-first-letter (string)
  "Doc string."
  (let (
        (first-letter (substring string 0 1))
        (remainder (substring string 1))
        )
    (concat (upcase first-letter) remainder)
    ))



;;
;;  shu-downcase-first-letter
;;
(defun shu-downcase-first-letter (string)
  "Doc string."
  (let (
        (first-letter (substring string 0 1))
        (remainder (substring string 1))
        )
    (concat (downcase first-letter) remainder)
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
        (class-name)
        (data-type)
        (full-data-type)
        (name)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (attr-info)
        (attributes)
        (z)
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
            (setq column-name (car x))
            (setq x (cdr x))
            (setq data-type (car x))
            (if (string= column-name "class")
                (setq class-name data-type)
              (setq x (cdr x))
              (setq name (car x))
              (setq nullable nil)
              (setq full-data-type data-type)
              (setq reference nil)
              (setq x (cdr x))
              (when x
                (setq z (car x))
                (princ "z: [" gb)(princ z gb)(princ "]\n" gb)
                (when (string=  z "&")
                  (setq reference t)
                  )
                )
              (when (cdr x)
                (setq nullable t)
                (setq full-data-type (shu-cpp-make-nullable data-type))
                )
              )
            )
          (when name
            (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment reference nullable column-name))
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
    (princ "class-name: " gb)(princ class-name gb) (princ "\n" gb)
    (shu-cpp-attributes-gen class-name attributes)
    ))


;;
;;  shu-cpp-attributes-gen
;;
(defun shu-cpp-attributes-gen (class-name attributes)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (sorted-attributes)
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
    (setq sorted-attributes (copy-tree attributes))
    (setq sorted-attributes (sort sorted-attributes 'shu-cpp-attributes-name-compare))
    (shu-cpp-attributes-gen-getter-has-decl sorted-attributes)
    (shu-cpp-attributes-gen-getter-decl sorted-attributes)
    (shu-cpp-attributes-gen-getter-has-gen class-name sorted-attributes)
    (shu-cpp-attributes-gen-getter-gen class-name sorted-attributes)
    (shu-cpp-attributes-gen-ctor-gen class-name attributes)
    (shu-cpp-attributes-gen-reset-gen class-name attributes)
    (shu-cpp-attributes-gen-set-values-gen class-name attributes)
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
        (column-name)
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
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (when (> (length full-data-type) max-type-len)
        (setq max-type-len (length full-data-type))
        )
      (setq attrs (cdr attrs))
      )
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
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
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (insert (concat "\n\n" ipad "// ACCESSORS\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (when nullable
        (insert "\n")
        (when comment
          (insert
           (concat "\n"
                   ipad "/*!\n"
                   ipad " * Return true if " name " exists\n"
                   ipad " */\n"))
          )
        (setq uname (shu-upcase-first-letter name))
        (insert (concat ipad "bool has" uname "() const;\n"))
        )
      (setq attrs (cdr attrs))
      )

    ))



;;
;;  shu-cpp-attributes-gen-getter-has-gen
;;
(defun shu-cpp-attributes-gen-getter-has-gen (class-name attributes)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (insert (concat "\n\n// ACCESSORS\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (when nullable
        (insert "\n\n")
        (setq uname (shu-upcase-first-letter name))
        (insert
         (concat
          "bool" class-name "::has" uname "() const\n"
          "{\n"
          ipad "return ( !(" member-prefix name ".isNull()) );\n"
          "}\n"
          ))
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
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (insert "\n")
      (when comment
        (insert
         (concat "\n"
                 ipad "/*!\n"
                 ipad " * Return " (shu-downcase-first-letter comment) "\n"))
        (when nullable
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (insert
           (concat
            ipad " *\n"
            ipad " * Behavior is undefined if " name " does not exist\n"
            ipad " * (" uname " returns false)\n"
            ))
          )
        (insert (concat ipad " */\n"))
        )
      (insert ipad)
      (when reference
        (insert "const ")
        )
      (insert (concat data-type " "))
      (when reference
        (insert "&")
        )
      (insert (concat name  "() const;\n"))
      (setq attrs (cdr attrs))
      )
    ))



;;
;;  shu-cpp-attributes-gen-getter-gen
;;
(defun shu-cpp-attributes-gen-getter-gen (class-name attributes)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (insert "\n\n")
      (when reference
        (insert "const ")
        )
      (insert (concat data-type " "))
      (when reference
        (insert "&")
        )
      (insert
       (concat
        class-name "::" name  "() const\n"
        "{\n"))
      (when nullable
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (insert
           (concat
            ipad "BSLS_ASSERT_OPT(" uname ");\n\n"
            ))
          )
      (insert
       (concat
        ipad "return " member-prefix name))
      (when nullable
        (insert ".value()")
        )
      (insert
       (concat
        ";\n"
        "}\n"
        ))
      (setq attrs (cdr attrs))
      )
    ))



;;
;;  shu-cpp-attributes-gen-ctor-gen
;;
(defun shu-cpp-attributes-gen-ctor-gen (class-name attributes)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        )
    (insert
     (concat
      "\n\n"
      "// CREATORS\n\n"
      class-name "::" class-name "(\n"
      ipad "bslma::Allocator    *allocator)\n"
      ":\n"
      ))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (insert (concat member-prefix name "("))
      (when (string= full-data-type "bsl::string")
        (insert "allocator")
        )
      (insert ")")
      (when (cdr attrs)
        (insert ",")
        )
      (insert "\n")
      (setq attrs (cdr attrs))
      )
    (insert
     (concat
      "{\n"
      ipad "reset();\n"
      "}\n"
      ))
    ))



;;
;;  shu-cpp-attributes-gen-reset-gen
;;
(defun shu-cpp-attributes-gen-reset-gen (class-name attributes)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (have-date)
        (have-interval)
        )
    (insert
     (concat
      "\n\n"
      "// MANIPULATORS\n\n"
      class-name "::reset()\n"
      "{\n"
      ))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (if (string= full-data-type "bdlt::Datetime")
          (setq have-date t)
        (when (string= full-data-type "bdlt::DatetimeInterval")
          (setq have-interval t)
          )
        )
      (setq attrs (cdr attrs))
      )
    (if have-interval
        (progn
          (insert (concat ipad "bdlt::DatetimeInterval  defaultInterval;\n"))
          (when have-date
            (insert (concat ipad "bdlt::Datetime          defaultTime;\n"))
            )
          )
      (when have-date
        (insert (concat ipad "bdlt::Datetime   defaultTime;\n"))
        )
      )
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (insert (concat ipad member-prefix name))
      (if nullable
          (insert ".reset()")
        (if (string= full-data-type "bsl::string")
            (insert ".clear()")
          (if (string= full-data-type "bdlt::Datetime")
              (insert " = defaultTime")
            (if (string= full-data-type "bdlt::DatetimeInterval")
                (insert " = defaultTime")
              (if (string= full-data-type "int")
                  (insert " = 0")
                (when (string= full-data-type "double")
                  (insert " = 0.0")
                    )
                  )
              )
            )
          )
        )
      (insert ";\n")
      (setq attrs (cdr attrs))
      )
    (insert "}\n")
    ))




;;
;;  shu-cpp-attributes-gen-set-values-gen
;;
(defun shu-cpp-attributes-gen-set-values-gen (class-name attributes)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (have-date)
        (have-interval)
        )
    (insert
     (concat
      "\n\n"
      class-name "::setValues)\n"
      ipad "const bsl::string       &databaseName,\n"
      ipad "const bcem_Aggregate    &data)\n"
      "{\n"
      ipad "BALL_LOG_SET_CATEGORY(__func__);\n"
      ipad "reset();\n"
      ipad "const bsl::string  why(\n"
      ipad "    \"This is caused by a query definition mis-match between this \"\n"
      ipad "    \"program and the definition of the table \"\n"
      ipad "    \"in database '\" + databaseName + \"'.\");\n"
      ipad "int fetchCount(0);\n"
      ipad "int missingCount(0);\n"
      ipad "const bsl::string  tableName(\"cross_outer_row join virtual\");\n"
      ))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name)
      (insert
       (concat
        ipad "const bcem_Aggregate &" name " = data[" column-name "];\n"
        ipad "if ( !" name ".isNul2() )\n"
        ipad "{\n"
        ipad ipad member-prefix name
        ))
      (if nullable
          (insert ".makeValue(")
        (insert " = ")
        )
      (insert (concat name ".as"))
        (if (string= data-type "bsl::string")
            (insert "String()")
          (if (string= data-type "bdlt::Datetime")
              (insert "DatetimeTz().utcDatetime()")
            (if (string= data-type "bdlt::DatetimeInterval")
                (insert "Int()")
              (if (string= data-type "int")
                  (insert "Int()")
                (when (string= data-type "double")
                  (insert "Double()")
                    )
                  )
              )
            )
          )
        (when nullable
          (insert ")")
          )
      (insert
       (concat
        ";\n"
        ipad ipad "fetchCount++;\n"
        ipad "}\n"
        ipad "else\n"
        ipad "{\n"
        ipad ipad "BALL_LOG_ERROR << \"No data found for input column '\"\n"
        ipad ipad "               << " column-name " << \"'. \" << why;\n"
        ipad ipad "missingCount++;\n"
        ipad "}\n"
      ))
      (setq attrs (cdr attrs))
      )
    (insert
     (concat
      ipad "if (missingCount)\n"
      ipad "{\n"
      ipad ipad "BALL_LOG_ERROR << \"Assertion failure will follow. \" << why;\n"
      ipad ipad "const int schema_mis_match_between_this_program_and_the_table_see_log_file(0);\n"
      ipad ipad "BSLS_ASSERT_OPT(schema_mis_match_between_this_program_and_the_table_see_log_file);\n"
      ipad "}\n"
      ipad "\n"
      ipad "return fetchCount;\n"
      "}\n"
      ))
    ))





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
        (column-name "MumbleBar::otherThing")
        (reference t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)
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

    (should xcolumn-name)
    (should (stringp xcolumn-name))
    (should (string= column-name xcolumn-name))

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
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)

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
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)

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
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)

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
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xreference)
        (xnullable)
        (xcolumn-name)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)
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
        (reference)
        (nullable t)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (setq xname (shu-cpp-extract-attr-info-name attr-info))
    (should xname)
    (should (stringp xname))
    (should (string= name xname))
    ))




;;
;;  shu-test-shu-upcase-first-letter-1
;;
(ert-deftest shu-test-shu-upcase-first-letter-1 ()
  (let (
        (phrase "now is the time")
        (expected "Now is the time")
        (actual)
        )
    (setq actual (shu-upcase-first-letter phrase))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-downcase-first-letter-1
;;
(ert-deftest shu-test-shu-downcase-first-letter-1 ()
  (let (
        (phrase "Now is the time")
        (expected "now is the time")
        (actual)
        )
    (setq actual (shu-downcase-first-letter phrase))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;; shu-attributes.el ends here
