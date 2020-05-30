;;; shu-attributes.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2020 Stewart L. Palmer
;;
;; Package: shu-attributes
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


;;  Stored information about each attribute (attr-info)
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
;;        |       +-----> attr-ct
;;        |
;;        +-------------> column-name
;;
;;
;;
;;
;;
;;  attr-ct
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-enu
;;        |
;;        +-------------> nullable & reference
;;
;;
;;
;;
;;
;;  attr-enu
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-dft
;;        |
;;        +-------------> enum-base
;;
;;
;;
;;
;;
;;  attr-dft
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> column-count
;;        |
;;        +-------------> reset-value
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
(defmacro shu-cpp-extract-attr-info (attr-info name data-type full-data-type comment
                                               reference nullable column-name column-count
                                               enum-base reset-value)
  "Extract the information out of an attr-info"
  (let (
        (tattr-ext (make-symbol "attr-ext"))
        (tattr-other (make-symbol "attr-other"))
        (tattr-cmt (make-symbol "attr-cmt"))
        (tattr-col (make-symbol "attr-col"))
        (tattr-ct (make-symbol "attr-ct"))
        (tattr-enu (make-symbol "attr-enu"))
        (tattr-dft (make-symbol "attr-dft"))
        (tflags (make-symbol "flags"))
        )
    `(let (
           (,tattr-ext)
           (,tattr-other)
           (,tattr-cmt)
           (,tattr-col)
           (,tattr-ct)
           (,tattr-enu)
           (,tattr-dft)
           (,tflags)
           )
       (setq ,name (car ,attr-info))
       (setq ,tattr-ext (cdr ,attr-info))
       (setq ,full-data-type (car ,tattr-ext))
       (setq ,tattr-other (cdr ,tattr-ext))
       (setq ,data-type (car ,tattr-other))
       (setq ,tattr-cmt (cdr ,tattr-other))
       (setq ,tattr-col (cdr ,tattr-cmt))
       (setq ,tattr-ct (cdr ,tattr-col))
       (setq ,tattr-enu (cdr ,tattr-ct))
       (setq ,tattr-dft (cdr ,tattr-enu))
       (setq ,comment (car ,tattr-cmt))
       (setq ,tflags (car ,tattr-ct))
       (if (= (logand ,tflags shu-cpp-attributes-reference) shu-cpp-attributes-reference)
           (setq ,reference t)
         (setq ,reference nil)
         )
       (if (= (logand ,tflags shu-cpp-attributes-nullable) shu-cpp-attributes-nullable)
           (setq ,nullable t)
         (setq ,nullable nil)
         )
       (setq ,column-name (car ,tattr-col))
       (setq ,enum-base (car ,tattr-enu))
       (setq ,column-count (cdr ,tattr-dft))
       (setq ,reset-value (car ,tattr-dft))
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
                                    reference nullable column-name column-count
                                    enum-base reset-value)
  "Return an attr-info created from the given arguments"
  (let ((flags 0))
    (when (not column-count)
      (setq column-count 1))
    (when reference
      (setq flags shu-cpp-attributes-reference))
    (when nullable
      (setq flags (logior flags shu-cpp-attributes-nullable)))
    (cons name
          (cons full-data-type
                (cons data-type
                      (cons comment
                            (cons column-name
                                  (cons flags
                                        (cons enum-base
                                              (cons reset-value column-count))))))))
    ))



;;
;;  shu-cpp-print-attr-info
;;
(defun shu-cpp-print-attr-info (attr-info buf)
  "Print the contents of ATTR-INFO into the buffer BUF"
  (interactive)
  (let ((column-name)
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-count)
        (enum-base)
        (penum-base)
        (reset-value)
        (preset-value))
    (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                               reference nullable column-name column-count
                               enum-base reset-value)
    (if enum-base
        (setq penum-base enum-base)
      (setq penum-base "nil"))
    (if reset-value
        (setq preset-value reset-value)
      (setq preset-value "nil"))
    (princ (concat
            "col: " column-name
            ", name: [" name "], type: [" data-type "], ref: " (shu-bool-to-string nullable)
            "], nullable: " (shu-bool-to-string nullable)
            ", full-type: [" full-data-type "]"
            ", count: " (number-to-string column-count)
            ", enum-base: " penum-base
            ", reset-value: " preset-value "\n") buf)
    (when comment
      (princ (concat "    [" comment "]\n") buf))
    ))



;;
;;  shu-cpp-attributes-bind-type
;;
(defun shu-cpp-attributes-bind-type (data-type)
  "Return the data type for the database bind.  \"string\" returns \"Text\",
\"int\" returns \"Int\", etc."
  (let ((bind-type "Text"))
    (cond
     ((string= data-type "bsl::string")
      (setq bind-type "Text"))
     ((string= data-type "bdlt::Datetime")
      (setq bind-type "Datetime"))
     ((string= data-type "bdlt::DatetimeTz")
      (setq bind-type "DatetimeTz"))
     ((string= data-type "bdlt::DatetimeInterval")
      (setq bind-type "Int"))
     ((string= data-type "bsls::Types::Int64")
      (setq bind-type "Int"))
     ((string= data-type "int")
      (setq bind-type "Int"))
     ((string= data-type "double")
      (setq bind-type "Double")))
    bind-type
    ))




;;
;;  shu-cpp-attributes-aggregate-type
;;
(defun shu-cpp-attributes-aggregate-type (data-type)
  "Return the data type for the set from aggregate value.  \"string\" returns
\"asString()\".  \"int\" returns \"asInt()\", etc."
  (let ((aggregate-type "asString()"))
    (cond
     ((string= data-type "bsl::string")
      (setq aggregate-type "asString()"))
     ((string= data-type "bdlt::Datetime")
      (setq aggregate-type "asDatetimeTz().utcDatetime()"))
     ((string= data-type "bdlt::DatetimeTz")
      (setq aggregate-type "asDatetimeTz()"))
     ((string= data-type "bdlt::DatetimeInterval")
      (setq aggregate-type "asInt()"))
     ((string= data-type "bsls::Types::Int64")
      (setq aggregate-type "asInt()"))
     ((string= data-type "int")
      (setq aggregate-type "asInt()"))
     ((string= data-type "double")
      (setq aggregate-type "asDouble()")))
    aggregate-type
    ))




;;
;;  shu-cpp-attributes-current-data-type
;;
(defun shu-cpp-attributes-current-data-type (data-type enum-base)
  "If ENUM-BASE is null, return DATA-TYPE.  if ENUM-BASE is non-nil, return ENUM-BASE."
  (let ((current-data-type (if enum-base enum-base data-type)))
    current-data-type
    ))



;;
;;  shu-cpp-attributes-header-type
;;
(defun shu-cpp-attributes-header-type (class-name data-type)
  "If DATA-TYPE is a name that is qualiified by CLASS-NAME, then return the
data type minus the class name qualification.  Otherwise, return the data type
unaltered."
  (let ((ss "\\([_a-zA-Z0-9$]+\\)::\\([_a-zA-Z0-9$]+\\)")
        (unqualified-type data-type)
        (first-part))
    (when (string-match ss data-type)
      (setq first-part (match-string-no-properties 1 data-type))
      (when (string= first-part class-name)
        (setq unqualified-type (match-string-no-properties 2 data-type))))
    unqualified-type
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
  (let ((lhs-name (shu-cpp-extract-attr-info-name lhs-attr-info))
        (rhs-name (shu-cpp-extract-attr-info-name rhs-attr-info)))
    (string< lhs-name rhs-name)
    ))



;;
;;  shu-attributes-internal-gen
;;
(defun shu-attributes-internal-gen (input-file output-file)
  "Doc string."
  (let (
        (class-list)
        (class-name)
        (table-name)
        (have-nullables)
        (have-non-nullables)
        (attributes)
        )
    (if (not (file-readable-p input-file))
        (message "%s: File not found." input-file)
      (when (file-readable-p output-file)
        (delete-file output-file)
        )
      (find-file input-file)
      (setq class-list (shu-attributes-fetch-attributes (point-min) (point-max)))
      (setq class-name (pop class-list))
      (setq table-name (pop class-list))
      (setq have-nullables (pop class-list))
      (setq have-non-nullables (pop class-list))
      (setq attributes (pop class-list))
      (find-file output-file)
      (shu-cpp-attributes-gen class-name table-name have-nullables
                              have-non-nullables attributes)
      )
    ))




;;
;;  shu-make-class
;;
(defun shu-make-class (start end)
  "Doc string."
  (interactive "r")
  (let (
        (class-list)
        (class-name)
        (table-name)
        (have-nullables)
        (have-non-nullables)
        (attributes)
        )
    (setq class-list (shu-attributes-fetch-attributes start end))
    (setq class-name (pop class-list))
    (setq table-name (pop class-list))
    (setq have-nullables (pop class-list))
    (setq have-non-nullables (pop class-list))
    (setq attributes (pop class-list))
    (shu-cpp-attributes-gen class-name table-name have-nullables
                            have-non-nullables attributes)
    ))




;;
;;  shu-attributes-fetch-attributes
;;
(defun shu-attributes-fetch-attributes (start end)
  "Create a row class for a table in a comdb2 database.

The columns are defined as a series of lines in a text file.  The first line
identifies the class name ns is simply

    class className

Blank lines are ignored.  Any line that starts with \"//\" is assumed to be a
comment for a following column definition.

Each column is has x attributes all on a single line

    1. The name of the variable that holds the column name
       If this name is \"std\", it may also have a column count in
       parenthesis as in \"std(5)\".  This indicates that this is not
       actually a column but is an instance of another class of this type
    2. The data type of the column
       If this data type is to be cast to a different type for inserting
       and fetching, then the database type is in parenthesis following
       the data type.  For example (\"Mumble(int)\" indicates that the data
       type Mumble is to be cast to an int for inserting and from an int
       back to Mumble when fetching.
    3. The name of the member variable that holds the column value
       If the member variable has a value that indicates it is in the \"reset\"
       or initial state, the value may follow the name in parenthesis.
       This value will be used in the generated reset() function to reset
       the variable to its initially constructed state.
    4. If this is \"&\" the value is passed by reference.  If this is absent or
       has any value other than \"&\", the value is passed by value.
    5. If this is present, the column is nullable

Mark the lines to be scanned and then invoke this function.  The generated code
snippets will be inserted into the same file.

Return a list that holds the following information:

    1. class-name
    2. table-name
    3. have-nullables (t if any nullable attributes exist)
    4. have-non-nullables (t if any non-nullable attributes exist)
    5. The list of attr-info that describes the attributes"
  (let ((gb (get-buffer-create "**boo**"))
        (sline (shu-the-line-at start))
        (eline (shu-the-line-at end))
        (line-diff 0)
        (eol)
        (line)
        (x)
        (ss "std\\s-*(\\([0-9]*\\))")
        (enum-ss "\\([:_a-zA-Z0-9$]+\\)\\s-*(\\([:_a-zA-Z0-9$]+\\))")
        (reset-ss "\\([:_a-zA-Z0-9$]+\\)\\s-*(\\([:._a-zA-Z0-9$]+\\))")
        (class-name)
        (table-name)
        (data-type)
        (full-data-type)
        (name)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-ct)
        (column-count)
        (enum-base)
        (reset-value)
        (attr-info)
        (attributes)
        (have-nullables)
        (have-non-nullables)
        (z))
    (while (and (<= (shu-current-line) eline) (= line-diff 0)) ; there are more lines
      (setq eol (line-end-position))
      (when (> eol (point))
        (setq line (shu-trim (buffer-substring-no-properties (point) eol)))
        (princ (concat "\n\nline: [" line "]\n") gb)
        (when (> (length line) 1)
          (if (string= (substring line 0 2) "//")
              (progn
                (setq comment (shu-trim (substring line 2)))
                (princ (concat "comment: [" comment "]\n") gb))
            (setq x (split-string line nil t))
            (setq column-name (car x))
            (setq column-count 1)
            (when (string-match ss column-name)
              (setq column-ct (match-string-no-properties 1 column-name))
              (setq column-count (string-to-number column-ct))
              (setq column-name "std"))
            (setq x (cdr x))
            (setq data-type (car x))
            (if (string= column-name "class")
                (setq class-name data-type)
              (if (string= column-name "table")
                  (setq table-name data-type)
                (setq enum-base nil)
                (when (string-match enum-ss data-type)
                  (setq enum-base (match-string-no-properties 2 data-type))
                  (setq data-type (match-string-no-properties 1 data-type)))
                (setq x (cdr x))
                (setq name (car x))
                (setq reset-value nil)
                (when (string-match reset-ss name)
                  (setq reset-value (match-string-no-properties 2 name))
                  (setq name (match-string-no-properties 1 name)))
                (setq nullable nil)
                (setq full-data-type data-type)
                (setq reference nil)
                (setq x (cdr x))
                (when x
                  (setq z (car x))
                  (princ "z: [" gb)(princ z gb)(princ "]\n" gb)
                  (when (string=  z "&")
                    (setq reference t)))
                (if (cdr x)
                    (progn
                      (setq nullable t)
                      (setq have-nullables t)
                      (setq full-data-type (shu-cpp-make-nullable data-type)))
                  (setq have-non-nullables t)))))
          (when name
            (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                                    reference nullable column-name column-count
                                                    enum-base reset-value))
            (push attr-info attributes)
            (shu-cpp-print-attr-info attr-info gb)
            (setq comment nil)
            (setq name nil))))
      (setq line-diff (forward-line 1)))
    (setq line-diff (forward-line 1))
    (setq attributes (nreverse attributes))
    (princ "class-name: " gb)(princ class-name gb) (princ "\n" gb)
    (list class-name table-name have-nullables have-non-nullables attributes)
    ))




;;
;;  shu-cpp-attributes-gen
;;
(defun shu-cpp-attributes-gen (class-name table-name have-nullables
                                          have-non-nullables attributes)
  "Generate all of the code snippets."
  (let ((gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (sorted-attributes))
    (princ  "\n\nAttributes:\n" gb)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-print-attr-info attr-info gb)
      (setq attrs (cdr attrs)))
    (save-excursion
      (princ (concat "nullables: " (shu-bool-to-string have-nullables) "\n") gb)
      (goto-char (point-max))
      (insert "\n\n")
      (shu-cpp-attributes-gen-decl class-name attributes)
      (setq sorted-attributes (copy-tree attributes))
      (setq sorted-attributes (sort sorted-attributes 'shu-cpp-attributes-name-compare))
      (shu-cpp-attributes-gen-ctor-decl class-name have-non-nullables attributes)
      (shu-cpp-attributes-gen-reset-decl)
      (when have-nullables
        (shu-cpp-attributes-gen-setter-decl class-name sorted-attributes))
      (shu-cpp-attributes-gen-getter-has-decl sorted-attributes)
      (shu-cpp-attributes-gen-getter-decl class-name sorted-attributes)
      (shu-cpp-attributes-gen-print-self-decl)
      (shu-cpp-attributes-gen-operator-equal-decl class-name)
      (shu-cpp-attributes-gen-ctor-gen class-name attributes)
      (when have-non-nullables
        (shu-cpp-attributes-gen-ctor-gen-full class-name attributes))
      (shu-cpp-attributes-gen-reset-gen class-name attributes)
      (shu-cpp-attributes-gen-set-values-gen class-name table-name attributes)
      (when have-nullables
        (shu-cpp-attributes-gen-setter-gen class-name sorted-attributes))
      (shu-cpp-attributes-gen-bind-values-gen class-name attributes)
      (shu-cpp-attributes-gen-getter-has-gen class-name sorted-attributes)
      (shu-cpp-attributes-gen-getter-gen class-name sorted-attributes)
      (shu-cpp-attributes-gen-print-self-gen class-name sorted-attributes)
      (shu-cpp-attributes-gen-operator-equal-gen class-name sorted-attributes))
    ))



;;
;;  shu-cpp-attributes-gen-decl
;;
(defun shu-cpp-attributes-gen-decl (class-name attributes)
  "Generate the declaration of the member variables."
  (let ((attrs attributes)
        (attr-info)
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-count)
        (enum-base)
        (reset-value)
        (max-type-len 31)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (end-attr-num 0)
        (attr-range)
        (pad-count 0)
        (pad)
        (member-prefix "m_"))
    (insert
     (concat
      "\n\n"
      "#include <fxcrossdb_crossrowprimarykey.h>\n"
      "\n"
      "#include <bdlb_nullablevalue.h>\n"
      "#include <bdlt_datetime.h>\n"
      "#include <bdlt_datetimeinterval.h>\n"
      "#include <bsl_string.h>\n"
      "\n\n" ipad "// DATA\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (when (> (length full-data-type) max-type-len)
        (setq max-type-len (length full-data-type)))
      (setq attrs (cdr attrs)))
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (setq full-data-type (shu-cpp-attributes-header-type class-name full-data-type))
      (insert "\n")
      (when comment
        (setq attr-range (number-to-string attr-num))
        (when (/= column-count 1)
          (setq end-attr-num (+ attr-num (1- column-count)))
          (setq attr-range (concat (number-to-string attr-num) " - " (number-to-string end-attr-num))))
        (insert (concat ipad "//! " comment " (" attr-range ")\n"))
        (setq pad-count 0)
        (when (< (length full-data-type) max-type-len)
          (setq pad-count (- max-type-len (length full-data-type)))))
      (setq pad-count (+ pad-count 3))
      (setq pad (make-string pad-count ? ))
      (insert (concat ipad full-data-type pad member-prefix name ";\n"))
      (setq attr-num (+ attr-num column-count))
      (setq attrs (cdr attrs)))
    ))



;;
;;  shu-cpp-attributes-gen-ctor-decl
;;
(defun shu-cpp-attributes-gen-ctor-decl (class-name have-non-nullables attributes)
  "Generate the declarations for the functions that set attribute values."
  (let ((attrs attributes)
        (attr-info)
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-count)
        (enum-base)
        (reset-value)
        (header-data-type)
        (pad)
        (max-type-len (length "bslma::Allocator"))
        (ipad (make-string shu-cpp-indent-length ? ))
        (amper)
        (member-prefix "m_"))
    (insert
     (concat
      "\n"
      "\n"
      ipad "// CREATORS\n"
      "\n"
      ipad "/*!\n"
      ipad " * \\brief Create an empty " class-name " object\n"
      ipad " *\n"
      ipad " * Its values are expected to be set by the setValues() function from the\n"
      ipad " * data contained in an instance of bsidb2::Cursor.\n"
      ipad " */\n"
      ipad "explicit " class-name "(\n"
      ipad ipad "bslma::Allocator   *allocator = 0);\n"))
    (when have-non-nullables
      (insert
       (concat
        "\n"
        "\n"
        ipad "/*!\n"
        ipad " * \\brief Create a " class-name " object with values for all of\n"
        ipad " *         the non-null columns\n"
        ipad " */\n"
        ipad "explicit " class-name "(\n"))
      (while attrs
        (setq attr-info (car attrs))
        (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                   reference nullable column-name column-count
                                   enum-base reset-value)
        (when (not nullable)
          (setq header-data-type (shu-cpp-attributes-header-type class-name data-type))
          (when ( > (length header-data-type) max-type-len)
            (setq max-type-len (length header-data-type))))
        (setq attrs (cdr attrs)))
      (setq attrs attributes)
      (while attrs
        (setq attr-info (car attrs))
        (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                   reference nullable column-name column-count
                                   enum-base reset-value)
        (when (not nullable)
          (setq header-data-type (shu-cpp-attributes-header-type class-name data-type))
          (setq pad (shu-cpp-attributes-make-pad max-type-len reference header-data-type))
          (insert (concat ipad ipad "const "header-data-type pad name ",\n")))
        (setq attrs (cdr attrs)))
      (setq header-data-type "bslma::Allocator")
      (setq name "allocator")
      (setq pad (shu-cpp-attributes-make-pad max-type-len nil header-data-type))
      (setq pad (concat pad "     "))
      (insert (concat ipad ipad header-data-type pad "*" name " = 0);\n")))
    ))



;;
;;  shu-cpp-attributes-make-pad
;;
(defun shu-cpp-attributes-make-pad (max-type-len reference data-type)
  "Return a pad string used to extend the DATA-TYPE name to the length of the
longest data type plus three in the list of attributes.  If REFERENCE is true,
append an ampersand to the end of the pad string.  If REFERENCE is false, append
a blank to the resulting pad string.  MAX-TYPE-LEN is the length of the longest
data type name in the list of attributes."
  (let ((pad-len 0)
        (pad))
    (when (< (length data-type) max-type-len)
      (setq pad-len (- max-type-len (length data-type))))
    (setq pad-len (+ pad-len 3))
    (setq pad (make-string pad-len ? ))
    (if reference
        (setq pad (concat pad "&"))
      (setq pad (concat pad " ")))
    pad
    ))



;;
;;  shu-cpp-attributes-gen-getter-has-decl
;;
(defun shu-cpp-attributes-gen-getter-has-decl (attributes)
  "Generate the declaration of the functions that indicate presence or absence
of nullable values."
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_"))
    (insert
     (concat
      "\n\n" ipad "// ACCESSORS\n\n"
      ipad "/*!\n"
      ipad " * \\brief Bind our values to column names\n"
      ipad " */\n"
      ipad "void bindValues(\n"
      ipad ipad "fxpricingdb::Binder   &binder)\n"
      ipad "const;\n"
      ))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (when nullable
        (insert "\n")
        (when comment
          (insert
           (concat "\n"
                   ipad "/*!\n"
                   ipad " * Return true if " name " exists\n"
                   ipad " */\n")))
        (setq uname (shu-upcase-first-letter name))
        (insert (concat ipad "bool has" uname "() const;\n")))
      (setq attrs (cdr attrs)))
    ))




;;
;;  shu-cpp-attributes-gen-bind-values-gen
;;
(defun shu-cpp-attributes-gen-bind-values-gen (class-name attributes)
  "Generate the code that binds all of the values to their column names."
  (let ((gb (get-buffer-create "**boo**"))
        (attrs attributes)
        (attr-info)
        (name)
        (uname)
        (data-type)
        (tmp-data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (have-date)
        (have-interval)
        (contained-class))
    (insert
     (concat
      "\n\n"
      "// ACCESSORS\n\n"
      "void " class-name "::bindValues(\n"
      ipad "fxpricingdb::Binder   &binder)\n"
      "const\n"
      "{\n"
      ))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (and (not nullable)
               (string= column-name "std"))
          (setq contained-class t)
        (setq contained-class nil))
      (if contained-class
          (insert (concat ipad member-prefix name ".bindValues(binder);\n"))
        (if (not nullable)
            (insert (concat ipad  "binder.bind"))
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (insert
           (concat
            ipad  "if ( !" uname " )\n"
            ipad ipad "binder.bindNull(\"@\" + "  column-name ", __FILE__, __LINE__);\n"
            ipad  "else\n"
            ipad ipad "binder.bind")))
        (setq tmp-data-type data-type)
        (when enum-base
          (setq tmp-data-type enum-base))
        (insert (shu-cpp-attributes-bind-type tmp-data-type))
        (insert (concat "(\"@\" + " column-name ", "))
        (if enum-base
            (insert (concat "static_cast<" enum-base ">(" name "())"))
          (insert (concat name "()")))
        (when (string= data-type "bdlt::DatetimeInterval")
          (insert ".totalMilliseconds()"))
        (insert ", __FILE__, __LINE__);\n"))
      (setq attrs (cdr attrs)))
    (insert "}\n")
    ))



;;
;;  shu-cpp-attributes-gen-getter-has-gen
;;
(defun shu-cpp-attributes-gen-getter-has-gen (class-name attributes)
  "Generate the code for the functions that indicate the presence or absence
of values for individual nullable columns."
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (when nullable
        (insert "\n\n")
        (setq uname (shu-upcase-first-letter name))
        (insert
         (concat
          "bool " class-name "::has" uname "() const\n"
          "{\n"
          ipad "return ( !(" member-prefix name ".isNull()) );\n"
          "}\n"
          )))
      (setq attrs (cdr attrs)))
    ))



;;
;;  shu-cpp-attributes-gen-reset-decl
;;
(defun shu-cpp-attributes-gen-reset-decl ()
  "Generate the declarations for the two manipulator functions."
  (let ((gb (get-buffer-create "**boo**"))
        (ipad (make-string shu-cpp-indent-length ? )))
    (insert
     (concat
      "\n\n"
      ipad "// MANIPULATORS\n\n"
      ipad "/*!\n"
      ipad " * \\brief Reset this object to the newly constructed state\n"
      ipad " */\n"
      ipad "void reset();\n"
      ipad "\n"
      ipad "\n"
      ipad "/*!\n"
      ipad " * \\brief Reset the state of this object to the newly constructed state\n"
      ipad " *        and then set all of the attributes of this object from the\n"
      ipad " *        given bcem_Aggregate\n"
      ipad " *\n"
      ipad " * The database name is provided for inclusion in error logs.\n"
      ipad " * The table name is included for the same purpose because this object\n"
      ipad " * sets values from any table that contains this key as either a\n"
      ipad " * primary or foreign key.\n"
      ipad " *\n"
      ipad " * Return the number of attributes that were actually set, not including\n"
      ipad " * those values in the bcem_Aggregate that were null.\n"
      ipad " *\n"
      ipad " * Assertion failure if any expected column values are missing.  Missing\n"
      ipad " * column data can only be caused by a mis-match between this program and\n"
      ipad " * the schema definition of the database.  This is a fatal error for which\n"
      ipad " * no recovery is possible.  Most programs do not even detect this error,\n"
      ipad " * and, instead, use undefined values when column data are missing.\n"
      ipad " *\n"
      ipad " */\n"
      ipad "int setValues(\n"
      ipad ipad "const bsl::string       &databaseName,\n"
      ipad ipad "const bcem_Aggregate    &data);\n"
      ))
    ))



;;
;;  shu-cpp-attributes-gen-getter-decl
;;
(defun shu-cpp-attributes-gen-getter-decl (class-name attributes)
  "Generate the declarations for the functions that return attribute values."
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (ref)
        (member-prefix "m_"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (insert "\n")
      (when comment
        (setq ref "")
        (when reference
          (setq ref "a reference to "))
        (insert
         (concat "\n"
                 ipad "/*!\n"
                 ipad " * Return " ref (shu-downcase-first-letter comment) "\n"))
        (when nullable
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (insert
           (concat
            ipad " *\n"
            ipad " * Behavior is undefined if " name " does not exist\n"
            ipad " * (" uname " returns false)\n")))
        (insert (concat ipad " */\n")))
      (insert ipad)
      (when reference
        (insert "const "))
      (insert (concat (shu-cpp-attributes-header-type class-name data-type) " "))
      (when reference
        (insert "&"))
      (insert (concat name  "() const;\n"))
      (setq attrs (cdr attrs)))
    ))



;;
;;  shu-cpp-attributes-gen-getter-gen
;;
(defun shu-cpp-attributes-gen-getter-gen (class-name attributes)
  "Generate the code for the functions that return attribute values."
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (insert "\n\n")
      (when reference
        (insert "const "))
      (insert (concat data-type " "))
      (when reference
        (insert "&"))
      (insert
       (concat
        class-name "::" name  "() const\n"
        "{\n"))
      (when nullable
        (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
        (insert
         (concat
          ipad "BSLS_ASSERT_OPT(" uname ");\n\n")))
      (insert
       (concat
        ipad "return " member-prefix name))
      (when nullable
        (insert ".value()"))
      (insert
       (concat
        ";\n"
        "}\n"))
      (setq attrs (cdr attrs)))
    ))




;;
;;  shu-cpp-attributes-gen-print-self-gen
;;
(defun shu-cpp-attributes-gen-print-self-gen (class-name attributes)
  "Generate the code that binds all of the values to their column names."
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (lpad "")
        (comma "")
        (semi "")
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (have-date)
        (have-interval)
        (contained-class))
    (setq lpad (concat ipad "os "))
    (insert
     (concat
      "\n"
      "\n"
      "bsl::ostream &" class-name "::printSelf(\n"
      "    bsl::ostream    &os)\n"
      "const\n"
      "{\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (and (not nullable)
               (string= column-name "std"))
          (setq contained-class t)
        (setq contained-class nil))
      (when contained-class
        (insert (concat ipad name "().printSelf(os);\n")))
      (setq attrs (cdr attrs)))
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (and (not nullable)
               (string= column-name "std"))
          (setq contained-class t)
        (setq contained-class nil))
      (when (not contained-class)
        (if (not nullable)
            (progn
              (insert (concat semi lpad "<< \"" comma name ": \" << " name "()"))
              (setq lpad (concat ipad "   "))
              (setq comma ", ")
              (setq semi "\n"))
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (setq lpad (concat ipad "os "))
          (when (/= (length semi) 0)
            (setq semi ";\n"))
          (insert
           (concat
            semi
            ipad "if (" uname ")\n"
            ipad ipad "os << \"" comma name ": \" << " name "();\n"))
          (setq comma ", ")
          (setq semi "")))
      (when (and (not nullable) (not (cdr attrs)))
        (insert ";\n"))
      (setq attrs (cdr attrs)))
    (insert
     (concat
      "\n"
      ipad "return os;\n"
      "}\n"))
    ))



;;
;;  shu-cpp-attributes-gen-ctor-gen
;;
(defun shu-cpp-attributes-gen-ctor-gen (class-name attributes)
  "Generate the code for the constructor."
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (contained-class))
    (insert
     (concat
      "\n\n\n"
      "#include <fxcrossdb_tenorcolumnnames.h>\n"
      "\n"
      "#include <ball_log.h>\n"
      "\n\n\n"
      "// CREATORS\n\n"
      class-name "::" class-name "(\n"
      ipad "bslma::Allocator    *allocator)\n"
      ":\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (and (not nullable)
               (string= column-name "std"))
          (setq contained-class t)
        (setq contained-class nil))
      (insert (concat member-prefix name "("))
      (when (or (string= full-data-type "bsl::string")
                contained-class)
        (insert "allocator"))
      (when reset-value
        (insert reset-value))
      (insert ")")
      (when (cdr attrs)
        (insert ","))
      (insert "\n")
      (setq attrs (cdr attrs)))
    (insert
     (concat
      "{\n"
      ipad "reset();\n"
      "}\n"))
    ))



;;
;;  shu-cpp-attributes-gen-ctor-gen-full
;;
(defun shu-cpp-attributes-gen-ctor-gen-full (class-name attributes)
  "Generate the code for the constructor that sets all of the non-nullable
attributes."
  (let ((attrs attributes)
        (attr-info)
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-count)
        (enum-base)
        (reset-value)
        (header-data-type)
        (pad)
        (max-type-len (length "bslma::Allocator"))
        (ipad (make-string shu-cpp-indent-length ? ))
        (amper)
        (member-prefix "m_")
        (contained-class))
    (insert
     (concat
      "\n"
      "\n"
      class-name "::" class-name "(\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (when (not nullable)
        (when ( > (length data-type) max-type-len)
          (setq max-type-len (length data-type))))
      (setq attrs (cdr attrs)))
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (when (not nullable)
        (setq data-type (shu-cpp-attributes-header-type class-name data-type))
        (setq pad (shu-cpp-attributes-make-pad max-type-len reference data-type))
        (insert (concat ipad "const " data-type pad name ",\n")))
      (setq attrs (cdr attrs)))
    (setq data-type "bslma::Allocator")
    (setq name "allocator")
    (setq pad (shu-cpp-attributes-make-pad max-type-len nil data-type))
    (setq pad (concat pad "     "))
    (insert (concat ipad data-type pad "*" name ")\n:\n"))
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (and (not nullable)
               (string= column-name "std"))
          (setq contained-class t)
        (setq contained-class nil))
      (insert (concat member-prefix name "("))
      (when (not nullable)
        (insert name))
      (when (string= full-data-type "bsl::string")
        (insert ", allocator"))
      (insert ")")
      (when (cdr attrs)
        (insert ","))
      (insert "\n")
      (setq attrs (cdr attrs)))
    (insert
     (concat
      "{\n"
      "}\n"))
    ))



;;
;;  shu-cpp-attributes-gen-reset-gen
;;
(defun shu-cpp-attributes-gen-reset-gen (class-name attributes)
  "Generate the code for the reset function"
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (have-date)
        (have-date-tz)
        (have-interval)
        (contained-class))
    (insert
     (concat
      "\n\n"
      "// MANIPULATORS\n\n"
      "void " class-name "::reset()\n"
      "{\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (string= full-data-type "bdlt::Datetime")
          (setq have-date t)
        (if (string= full-data-type "bdlt::DatetimeTz")
            (setq have-date-tz t)
          (when (string= full-data-type "bdlt::DatetimeInterval")
            (setq have-interval t))))
      (setq attrs (cdr attrs)))
    (if have-interval
        (progn
          (insert (concat ipad "bdlt::DatetimeInterval  defaultInterval;\n"))
          (when have-date
            (insert (concat ipad "bdlt::Datetime          defaultTime;\n")))
          (when have-date-tz
            (insert (concat ipad "bdlt::DatetimeTz        defaultTimeTz;\n"))))
      (when have-date
        (insert (concat ipad "bdlt::Datetime     defaultTime;\n")))
      (when have-date-tz
        (insert (concat ipad "bdlt::DatetimeTz   defaultTimeTz;\n"))))
    (setq attrs attributes)
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (and (not nullable)
               (string= column-name "std"))
          (setq contained-class t)
        (setq contained-class nil))
      (insert (concat ipad member-prefix name))
      (if (or nullable contained-class)
          (insert ".reset()")
        (if (string= full-data-type "bsl::string")
            (insert ".clear()")
          (if (string= full-data-type "bdlt::Datetime")
              (insert " = defaultTime")
            (if (string= full-data-type "bdlt::DatetimeTz")
                (insert " = defaultTimeTz")
              (if (string= full-data-type "bdlt::DatetimeInterval")
                  (insert " = defaultInterval")
                (if (string= full-data-type "int")
                    (insert " = 0")
                  (if (string= full-data-type "double")
                      (insert " = 0.0")
                    (when reset-value
                      (insert (concat " = " reset-value))))))))))
      (insert ";\n")
      (setq attrs (cdr attrs)))
    (insert "}\n")
    ))



;;
;;  shu-cpp-attributes-gen-setter-decl
;;
(defun shu-cpp-attributes-gen-setter-decl (class-name attributes)
  "Generate the declarations for the functions that set attribute values."
  (let ((attrs attributes)
        (attr-info)
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (amper))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (when nullable
        (insert "\n")
        (when comment
          (insert
           (concat
            "\n"
            ipad "/*!\n"
            ipad " * Set " (shu-downcase-first-letter comment) "\n"))
          (insert (concat ipad " */\n")))
        (setq amper "")
        (when reference
          (setq amper "&"))
        (insert
         (concat
          ipad "void set" (shu-upcase-first-letter name) "(\n"
          ipad ipad "const " (shu-cpp-attributes-header-type class-name data-type) "   " amper name ");\n")))
      (setq attrs (cdr attrs)))
    ))




;;
;;  shu-cpp-attributes-gen-set-values-gen
;;
(defun shu-cpp-attributes-gen-set-values-gen (class-name table-name attributes)
  "Generate the code for the setValues function that sets all of the member variable
values from an instance of bcem_Aggregate."
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (have-date)
        (have-interval)
        (contained-class))
    (insert
     (concat
      "\n\n"
      "int " class-name "::setValues(\n"
      ipad "const bsl::string       &databaseName,\n"
      ipad "const bcem_Aggregate    &data)\n"
      "{\n"
      ipad "BALL_LOG_SET_CATEGORY(__func__);\n"
      ipad "reset();\n"
      ipad "const bsl::string  why(\n"
      ipad "    \"This is caused by a query definition mis-match between this \"\n"
      ipad "    \"program and the definition of the table \" + " table-name " +\n"
      ipad "    \" in database '\" + databaseName + \"'.\");\n"
      ipad "const bsl::string  tableName(" table-name ");\n"
      ipad "int fetchCount(0);\n"
      ipad "int missingCount(0);\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if (and (not nullable)
               (string= column-name "std"))
          (setq contained-class t)
        (setq contained-class nil))
      (if contained-class
          (insert (concat ipad "fetchCount += " member-prefix name ".setValues(databaseName, tableName, data);\n"))
        (insert
         (concat
          ipad "const bcem_Aggregate &" name " = data[" column-name "];\n"
          ipad "if ( !" name ".isNul2() )\n"
          ipad "{\n"))
        (when (string= data-type "bdlt::DatetimeInterval")
          (insert
           (concat
            ipad ipad "const bsls::Types::Int64      intval(" name ".asInt());\n"
            ipad ipad "const bdlt::DatetimeInterval  interval(0, 0, 0, 0, intval, 0);\n")))
        (insert
         (concat
          ipad ipad member-prefix name))
        (if nullable
            (insert ".makeValue(")
          (insert " = "))
        (if (string= data-type "bdlt::DatetimeInterval")
            (insert "interval")
          (if enum-base
              (insert (concat "static_cast<" data-type ">(" name "." (shu-cpp-attributes-aggregate-type enum-base) ")"))
            (insert (concat name "." (shu-cpp-attributes-aggregate-type data-type)))))
        (when nullable
          (insert ")"))
        (insert
         (concat
          ";\n"
          ipad ipad "fetchCount++;\n"
          ipad "}\n"))
        (when (not nullable)
          (insert
           (concat
            ipad "else\n"
            ipad "{\n"
            ipad ipad "BALL_LOG_ERROR << \"No data found for input column '\"\n"
            ipad ipad "               << " column-name " << \"'. \" << why;\n"
            ipad ipad "missingCount++;\n"
            ipad "}\n"))))
      (setq attrs (cdr attrs)))
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
      "}\n"))
    ))



;;
;;  shu-cpp-attributes-gen-setter-gen
;;
(defun shu-cpp-attributes-gen-setter-gen (class-name attributes)
  "Generate the code for the functions that set attribute values."
  (let ((attrs attributes)
        (attr-info)
        (name)
        (data-type)
        (full-data-type)
        (comment)
        (reference)
        (nullable)
        (column-name)
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (amper)
        (member-prefix "m_"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (when nullable
        (setq amper "")
        (when reference
          (setq amper "&"))
        (insert
         (concat
          "\n"
          "\n"
          "void " class-name "::set" (shu-upcase-first-letter name) "(\n"
          ipad "const " data-type "   " amper name ")\n"
          "{\n"
          ipad member-prefix name ".makeValue(" name ");\n"
          "}\n"
          )))
      (setq attrs (cdr attrs)))
    ))




;;
;;  shu-cpp-attributes-gen-print-self-gen
;;
(defun shu-cpp-attributes-gen-print-self-decl ()
  "Generate the declaration for the printSelf function"
  (let((ipad (make-string shu-cpp-indent-length ? )))
  (insert
   (concat
    "\n"
    "\n"
    ipad "/*!\n"
    ipad " *  \\brief Stream object out to a stream\n"
    ipad " *\n"
    ipad " * Intended for use by operator<<()\n"
    ipad " */\n"
    ipad "bsl::ostream &printSelf(\n"
    ipad ipad "bsl::ostream    &os)\n"
    ipad "const;\n"))
  ))




;;
;;  shu-cpp-attributes-gen-operator-equal-decl
;;
(defun shu-cpp-attributes-gen-operator-equal-decl (class-name)
  "Generate the declaration for operator==()"
  (insert
   (concat
    "\n"
    "\n"
    "// FREE OPERATORS\n"
    "\n"
    "\n"
    "/*!\n"
    " * Return true if all attributes of `lhs` and `rhs` have the same value\n"
    " */\n"
    "bool operator==(\n"
    "    const " class-name "   &lhs,\n"
    "    const " class-name "   &rhs);\n"
    "\n"
    "\n"
    "/*!\n"
    " * Return true if any attributes of `lhs` and `rhs` have different values\n"
    " */\n"
    "bool operator!=(\n"
    "    const " class-name "   &lhs,\n"
    "    const " class-name "   &rhs);\n"
    ))
  )



;;
;;  shu-cpp-attributes-gen-operator-equal-gen
;;
(defun shu-cpp-attributes-gen-operator-equal-gen (class-name attributes)
  "Generate the code for operator==()"
  (let ((gb (get-buffer-create "**boo**"))
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
        (column-count)
        (enum-base)
        (reset-value)
        (ipad (make-string shu-cpp-indent-length ? ))
        (lpad)
        (attr-num 1)
        (pad-count 0)
        (pad)
        (member-prefix "m_")
        (have-nullable)
        (longest-name-length 0)
        (longest-nullable-name-length 0)
        (longest-non-nullable-name-length 0)
        (last-name)
        (last-nullable-name)
        (last-non-nullable-name))
    (insert
     (concat
      "\n"
      "\n"
      "// FREE OPERATORS\n"
      "\n"
      "bool operator==(\n"
      "    const " class-name "   &lhs,\n"
      "    const " class-name "   &rhs)\n"
      "{\n"
      "    bool  isSame(false);\n"))
    (while attrs
      (setq attr-info (car attrs))
      (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                 reference nullable column-name column-count
                                 enum-base reset-value)
      (if nullable
          (progn
            (setq have-nullable t)
            (setq last-nullable-name name)
            (when (> (length name) longest-nullable-name-length)
              (setq longest-nullable-name-length (length name))))
        (when (> (length name) longest-non-nullable-name-length)
          (setq longest-non-nullable-name-length (length name)))
        (setq last-non-nullable-name name))
      (when (> (length name) longest-name-length)
        (setq longest-name-length (length name)))
      (setq last-name name)
      (setq attrs (cdr attrs)))
    (setq attrs attributes)
    (if (not have-nullable)
        ;;; Generat operator==() for a class with no nullable values
        (progn
          (setq lpad "")
          (insert (concat ipad "if ("))
          (while attrs
            (setq attr-info (car attrs))
            (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                       reference nullable column-name column-count
                                       enum-base reset-value)
            (setq attr-info (car attrs))
            (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                       reference nullable column-name column-count
                                       enum-base reset-value)
            (setq pad-count 0)
            (when (< (length name) longest-name-length)
              (setq pad-count (- longest-name-length (length name))))
            (setq pad-count (1+ pad-count))
            (setq pad (make-string pad-count ? ))
            (insert (concat lpad "(lhs." name "()" pad "== rhs." name "())"))
            (if (not (string= name last-name))
                (insert (concat pad "&&"))
              (insert ")"))
            (insert "\n")
            (setq lpad (concat ipad ipad))
            (setq attrs (cdr attrs)))
          (insert
           (concat
            ipad "{\n"
            ipad ipad "isSame = true;\n"
            ipad "}\n"
            "\n"
            ipad "return isSame;\n"
            "}\n")))
      ;;; Generat operator==() for a class with nullable values

      ;;; First - Comparison for equality of all of the hasValue() functions
      (insert (concat ipad "if ("))
      (setq lpad "")
      (while attrs
        (setq attr-info (car attrs))
        (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                   reference nullable column-name column-count
                                   enum-base reset-value)
        (when nullable
          (setq pad-count 0)
          (when (< (length name) longest-nullable-name-length)
            (setq pad-count (- longest-nullable-name-length (length name))))
          (setq pad-count (1+ pad-count))
          (setq pad (make-string pad-count ? ))
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (insert (concat lpad "(lhs." uname pad "== rhs." uname ")"))
          (if (not (string= name last-nullable-name))
              (insert (concat pad "&&"))
            (insert ")"))
          (insert "\n")
          (setq lpad (concat ipad ipad)))
        (setq attrs (cdr attrs)))
      (insert
       (concat
        ipad "{\n"
        ipad ipad "isSame = true;\n"
        ipad "}\n"
        ipad "if (isSame)\n"
        ipad "{\n"
        ipad ipad "isSame = false;\n"
        ipad ipad "if ("))

      ;;; Second - Comparison for equality of all of the non-nullable values
      (setq attrs attributes)
      (setq lpad "")
      (while attrs
        (setq attr-info (car attrs))
        (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                   reference nullable column-name column-count
                                   enum-base reset-value)
        (when (not nullable)
          (setq pad-count 0)
          (when (< (length name) longest-non-nullable-name-length)
            (setq pad-count (- longest-non-nullable-name-length (length name))))
          (setq pad-count (1+ pad-count))
          (setq pad (make-string pad-count ? ))
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (insert (concat lpad "(lhs." name "()" pad "== rhs." name "())"))
          (if (not (string= name last-non-nullable-name))
              (insert (concat pad "&&"))
            (insert ")"))
          (insert "\n")
          (setq lpad (concat ipad ipad ipad)))
        (setq attrs (cdr attrs)))
      (insert
       (concat
        ipad ipad "{\n"
        ipad ipad ipad "isSame = true;\n"
        ipad ipad "}\n"
        ipad ipad "if (isSame)\n"
        ipad ipad "{\n"
        ipad ipad ipad "isSame = false;\n"
        ipad ipad ipad "if ("))

      ;;; Third - Comparison of all of the nullable values that have values
      (setq attrs attributes)
      (setq lpad " ")
      (while attrs
        (setq attr-info (car attrs))
        (shu-cpp-extract-attr-info attr-info name data-type full-data-type comment
                                   reference nullable column-name column-count
                                   enum-base reset-value)
        (when nullable
          (setq uname (concat "has" (shu-upcase-first-letter name) "()"))
          (insert (concat lpad "( ( !lhs." uname " ) ||\n"))
          (setq lpad (concat ipad ipad ipad ipad " "))
          (insert
           (concat
            lpad "  (  lhs." uname  " &&\n"
            lpad "    (lhs." name "() == rhs." name "() ) ) )"
            ))
          (if (not (string= name last-nullable-name))
              (insert (concat " &&"))
            (insert " )"))
          (insert "\n"))
        (setq attrs (cdr attrs)))
      (insert
       (concat
        ipad ipad ipad "{\n"
        ipad ipad ipad ipad "isSame = true;\n"
        ipad ipad ipad "}\n"
        ipad ipad "}\n"
        ipad "}\n"
        "\n"
        ipad "return isSame;\n"
        "}\n")))

    (insert
     (concat
      "\n"
      "\n"
      "bool operator!=(\n"
      "    const " class-name "   &lhs,\n"
      "    const " class-name "   &rhs)\n"
      "{\n"
      "    return  ( !operator==(lhs, rhs) );\n"
      "}\n"))
    ))


;;; shu-attributes.el ends here
