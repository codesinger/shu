;;; shu-cpp-token.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-cpp-token
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

;; Functions to parse a region of C++ code and return a list of tokens
;; found therein.  The returned list is a list of token-info, whose structure
;; is shown below.
;;
;; The two top level functions in this file are shu-cpp-tokenize-region and
;; shu-cpp-reverse-tokenize-region.  The former returns a list of tokens with the
;; first token in the list being the first token found.  The latter function
;; returns the reverse of the former.

;;; Code:

(provide 'shu-cpp-token)
(require 'shu-base)
;;
;;  token-info:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Next in list
;;        |
;;        +-------------> token-info
;;
;;
;;
;;  token-info:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> token
;;        |
;;        +-------------> info
;;
;;
;;  info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> ext-info
;;        |
;;        +-------------> token-type
;;
;;
;;
;;
;;  ext-info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> point-pair
;;        |
;;        +-------------> error message
;;
;;
;;  point-pair:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> End point
;;        |
;;        +-------------> Start point
;;
;;


(defvar shu-cpp-keywords-hash nil
  "The hash table of C++ key words")

(defconst shu-cpp-operators-three
  (regexp-opt (list "->*" "<<=" ">>=") nil)
  "Define the set of three character C++ operators")

(defconst shu-cpp-operators-two
  (regexp-opt (list
               "::" "++" "--" "->" "++" "--" ".*" "<<" ">>" "<=" ">=" "=="
               "!=" "&&" "||" "+=" "-=" "*=" "/=" "%=" "&=" "~=" "|=") nil )
  "Define the set of two character C++ operators")

(defconst shu-cpp-operators-one
  (regexp-opt (list
               "(" ")" "[" "]" "." "+" "-" "!" "~" "*" "&" "*" "#"
               "/" "%" "<" ">" "&" "^" "|" "?" ":" "=" "," ";" "{" "}") nil )
  "Define the set of one character operators.  Note that we include ; as
an operator, even though, strictly speaking, it is not an operator.")

(defconst shu-cpp-operator-start-chars
  (list
   "!" "%" "&" "(" ")" "*" "+" "," "-" "." "/" ":" "<" "=" ">" "?" "[" "]" "^" "|" "~" ";" "{" "}")
  "Define the set of characters that start C++ operators")

(defconst shu-cpp-operator-start
  (regexp-opt shu-cpp-operator-start-chars nil)
  "Define the set of characters that start C++ operators")

(defconst shu-cpp-token-delimiter-chars
  (append shu-all-whitespace-chars shu-cpp-operator-start-chars nil)
  "List of all of the characters that terminate an unquoted C++ token")

(defconst shu-cpp-token-type-op 1
  "Token type that indicates an operator")

(defconst shu-cpp-token-type-qt 2
  "Token type that indicates a quoted string")

(defconst shu-cpp-token-type-uq 3
  "Token type that indicates an unquoted token")

(defconst shu-cpp-token-type-kw 4
  "Token type that indicates a C++ key word")

(defconst shu-cpp-token-type-ct 5
  "Token type that indicates a comment")

(defconst shu-cpp-token-type-cc 6
  "Token type that indicates a line in which the first non-blank item is a
comment that starts in a column greater than or equal to the column defined
by shu-cpp-comment-start.  This is known as a code comment with no code present.")

(defconst shu-cpp-token-type-tp 7
  "Token type that indicates a template parameter.  The standard parsing does nothing
with template parameters.  Something like \"<int>\" is simply turned into three separate
tokens, \"<\", \"int\", and \">\" (or \">\", \"int\", and \"<\" in a reverse parse).
But some of the other transform functions will turn this list of tokens into the single
template parameter \"int\"")

(defconst shu-cpp-token-delimiter-end
  (regexp-opt shu-cpp-token-delimiter-chars  nil)
  "Regular expression to define that which terminates an unquoted token in C++")


;;
;;  shu-cpp-keywords
;;
(defconst shu-cpp-keywords
  (list
   (cons "alignas" 0)
   (cons "alignof" 0)
   (cons "and" 0)
   (cons "and_eq" 0)
   (cons "asm" 0)
   (cons "atomic_cancel" 0)
   (cons "atomic_commit" 0)
   (cons "atomic_noexcept" 0)
   (cons "auto" 0)
   (cons "bitand" 0)
   (cons "bitor" 0)
   (cons "bool" 0)
   (cons "break" 0)
   (cons "case" 0)
   (cons "catch" 0)
   (cons "char" 0)
   (cons "char8_t" 0)
   (cons "char16_t" 0)
   (cons "char32_t" 0)
   (cons "class" 0)
   (cons "compl" 0)
   (cons "concept" 0)
   (cons "const" 0)
   (cons "consteval" 0)
   (cons "constexpr" 0)
   (cons "const_cast" 0)
   (cons "continue" 0)
   (cons "co_await" 0)
   (cons "co_return" 0)
   (cons "co_yield" 0)
   (cons "decltype" 0)
   (cons "default" 0)
   (cons "delete" 0)
   (cons "do" 0)
   (cons "double" 0)
   (cons "dynamic_cast" 0)
   (cons "else" 0)
   (cons "enum" 0)
   (cons "explicit" 0)
   (cons "export" 0)
   (cons "extern" 0)
   (cons "false" 0)
   (cons "float" 0)
   (cons "for" 0)
   (cons "friend" 0)
   (cons "goto" 0)
   (cons "if" 0)
   (cons "import" 0)
   (cons "inline" 0)
   (cons "int" 0)
   (cons "long" 0)
   (cons "module" 0)
   (cons "mutable" 0)
   (cons "namespace" 0)
   (cons "new" 0)
   (cons "noexcept" 0)
   (cons "not" 0)
   (cons "not_eq" 0)
   (cons "nullptr" 0)
   (cons "operator" 0)
   (cons "or" 0)
   (cons "or_eq" 0)
   (cons "private" 0)
   (cons "protected" 0)
   (cons "public" 0)
   (cons "reflexpr" 0)
   (cons "register" 0)
   (cons "reinterpret_cast" 0)
   (cons "requires" 0)
   (cons "return" 0)
   (cons "short" 0)
   (cons "signed" 0)
   (cons "sizeof" 0)
   (cons "static" 0)
   (cons "static_assert" 0)
   (cons "static_cast" 0)
   (cons "struct" 0)
   (cons "switch" 0)
   (cons "synchronized" 0)
   (cons "template" 0)
   (cons "this" 0)
   (cons "thread_local" 0)
   (cons "throw" 0)
   (cons "true" 0)
   (cons "try" 0)
   (cons "typedef" 0)
   (cons "typeid" 0)
   (cons "typename" 0)
   (cons "union" 0)
   (cons "unsigned" 0)
   (cons "using" 0)
   (cons "virtual" 0)
   (cons "void" 0)
   (cons "volatile" 0)
   (cons "wchar_t" 0)
   (cons "while" 0)
   (cons "xor" 0)
   (cons "xor_eq" 0))
   "alist of C++ key words up to approximately C++17")


;;
;;  shu-get-cpp-keywords-hash
;;
(defun shu-get-cpp-keywords-hash ()
  "Return a hash table containing all of the C++ key words."
  (interactive)
  (let ((ht (make-hash-table :test 'equal :size (length shu-cpp-keywords)))
        (kl shu-cpp-keywords)
        (kc))
    (while kl
      (setq kc (car kl))
      (puthash (car kc) (cdr kc) ht)
      (setq kl (cdr kl)))
    ht
    ))


;;
;;  shu-cpp-token-extract-info
;;
(defmacro shu-cpp-token-extract-info (token-info token token-type spoint epoint error-message)
  "Extract the information out of a token-info"
  (let ((tinfo (make-symbol "info"))
        (text-info (make-symbol "ext-info"))
        (tpoint-pair (make-symbol "point-pair")))
    `(let ((,tinfo)
           (,text-info)
           (,tpoint-pair))
       (setq ,tinfo (car ,token-info))
       (setq ,token (cdr ,token-info))
       (setq ,token-type (car ,tinfo))
       (setq ,text-info (cdr ,tinfo))
       (setq ,error-message (car ,text-info))
       (setq ,tpoint-pair (cdr ,text-info))
       (setq ,spoint (car ,tpoint-pair))
       (setq ,epoint (cdr ,tpoint-pair)))
    ))


;;
;;  shu-cpp-token-extract-type
;;
(defsubst shu-cpp-token-extract-type (token-info)
  "Return the token type from an instance of token-info."
  (caar token-info)
  )


;;
;;  shu-cpp-token-extract-token
;;
(defsubst shu-cpp-token-extract-token (token-info)
  "Return the token from an instance of token-info."
  (cdr token-info)
  )


;;
;;  shu-cpp-token-extract-spoint
;;
(defsubst shu-cpp-token-extract-spoint (token-info)
  "Return the start point from an instance of token-info."
  (let ((info)
        (ext-info)
        (point-pair))
    (setq info (car token-info))
    (setq ext-info (cdr info))
    (setq point-pair (cdr ext-info))
    (car point-pair)
  ))


;;
;;  shu-cpp-token-extract-epoint
;;
(defsubst shu-cpp-token-extract-epoint (token-info)
  "Return the end point from an instance of token-info."
  (let ((info)
        (ext-info)
        (point-pair))
    (setq info (car token-info))
    (setq ext-info (cdr info))
    (setq point-pair (cdr ext-info))
    (cdr point-pair)
  ))


;;
;;  shu-cpp-token-is-comment
;;
(defsubst shu-cpp-token-is-comment (token-info)
  "Return true if the token-info represents a comment."
  (let ((ttype (shu-cpp-token-extract-type token-info)))
    (or (= ttype shu-cpp-token-type-ct)
        (= ttype shu-cpp-token-type-cc))
    ))



;;
;;  shu-cpp-token-next-non-comment
;;
(defun shu-cpp-token-next-non-comment (tlist)
  "TLIST points to a list of token-info.  Return TLIST pointing to the next
token-info that does not hold a comment.  If you are scanning through a list
of tokens, it is not uncommon to want to skip all of the comments.  Use this
at the bottom of the loop in place of the usual \"setq tlist (cdr tlist))\".

i.e.,

     (while tlist
        ...
       (setq tlist (cdr tlist)))

becomes

     (while tlist
        ...
       (setq tlist (shu-cpp-token-next-non-comment tlist)))

and you will scan through the list without seeing any comments."
  (let ((token-info)
        (in-comment t))
    (when tlist
      (setq tlist (cdr tlist))
      (while (and in-comment tlist)
        (setq token-info (car tlist))
        (setq in-comment (shu-cpp-token-is-comment token-info))
        (when in-comment
          (setq tlist (cdr tlist))
          )
        )
      )
    tlist
    ))



;;
;;  shu-cpp-token-first-non-comment
;;
(defun shu-cpp-token-first-non-comment (tlist)
  "TLIST points to a list of token-info.  Return TLIST pointing to the next
token-info that does not hold a comment.  If you are scanning through a list
of tokens, it is not uncommon to want to skip all of the comments.  Use this
at the bottom of the loop in place of the usual \"setq tlist (cdr tlist))\".

i.e.,

     (while tlist
        ...
       (setq tlist (cdr tlist)))

becomes

     (setq tlist (shu-cpp-token-first-non-comment tlist))
     (while tlist
        ...
       (setq tlist (shu-cpp-token-first-non-comment tlist)))

and you will scan through the list without seeing any comments."
  (let (
        (token-info)
        (in-comment)
        )
    (when tlist
      (setq token-info (car tlist))
      (setq in-comment (shu-cpp-token-is-comment token-info))
      (while (and in-comment tlist)
        (setq tlist (cdr tlist))
        (setq token-info (car tlist))
        (setq in-comment (shu-cpp-token-is-comment token-info))
        )
      )
    tlist
    ))


;;
;;  shu-cpp-token-token-type-name
;;
(defun shu-cpp-token-token-type-name (token-type)
  "Return the name of a token type."
  (let (
        (type-name "**unknown** (nil)")
        )
    (when token-type
      (if (not (numberp token-type))
          (setq type-name "**unknown** (Not a number)")
        (setq type-name (format "**unknown** (%d)" token-type))
        (cond
         ((= token-type shu-cpp-token-type-op)
          (setq type-name "operator"))
         ((= token-type shu-cpp-token-type-qt)
          (setq type-name "quoted-string"))
         ((= token-type shu-cpp-token-type-uq)
          (setq type-name "unquoted token"))
         ((= token-type shu-cpp-token-type-kw)
          (setq type-name "key word"))
         ((= token-type shu-cpp-token-type-ct)
          (setq type-name "comment"))
         ((= token-type shu-cpp-token-type-cc)
          (setq type-name "code comment"))
         ((= token-type shu-cpp-token-type-tp)
          (setq type-name "template parameter"))
         )))
    type-name
    ))



;;
;;  shu-cpp-parse-region
;;
(defun shu-cpp-parse-region (start end)
  "Parse the region between START and END into a list of all of the C++ tokens
contained therein, displaying the result in the Shu unit test buffer."
  (interactive "r")
  (let
      ((func 'shu-cpp-tokenize-region))
    (shu-cpp-token-internal-parse-region func start end)
    ))


;;
;;  shu-cpp-reverse-parse-region
;;
(defun shu-cpp-reverse-parse-region (start end)
  "Reverse parse the region between START and END into a list of all of the C++
tokens contained therein, displaying the result in the Shu unit test buffer."
  (interactive "r")
  (let
      ((func 'shu-cpp-reverse-tokenize-region))
    (shu-cpp-token-internal-parse-region func start end)
    ))


;;
;;  shu-cpp-token-internal-parse-region
;;
(defun shu-cpp-token-internal-parse-region (func start end)
  "Internal function to do a forward or reverse parse of the region between START
and END.  FUNC holds the function to be invoked to do the parse.  This would be
either SHU-CPP-TOKENIZE-REGION or SHU-CPP-REVERSE-TOKENIZE-REGION.  Once the
parse is complete, the token list is shown in the Shu unit test buffer.  If any
error is detected, it is displayed at the point at which the error was
detected."
  (let
      ((token-list)
       (ret-val)
       (error-token-info)
       (token)
       (token-type)
       (error-message)
       (spoint)
       (epoint)
       (emsg)
       (tbuf      (get-buffer-create shu-unit-test-buffer)))
    (setq ret-val (funcall func start end))
    (setq error-token-info (car ret-val))
    (setq token-list (cdr ret-val))
    (shu-cpp-tokenize-show-list token-list)
    (when error-token-info
      (shu-cpp-token-show-token-info error-token-info)
      (shu-cpp-token-extract-info error-token-info token token-type spoint epoint error-message)
      (setq emsg error-message)
      (when (not error-message)
        (setq emsg "Uknown error at point"))
      (goto-char spoint)
      (message "%s" emsg))
    ))


;;
;;  shu-cpp-token-find-spanning-info-token
;;
(defun shu-cpp-token-find-spanning-info-token (token-list here-point)
  "Find the token-info in TOKEN-LIST that spans HERE-POINT, if any.  If there
is no such token-info return nil.  If there is such a token-info, return a
cons cell whose car is the spanning token-info and whose cdr is a pointer
to the next token-info in the tlist."
  (let
      ((tlist token-list)
       (done)
       (first-spoint)
       (ret-val)
       (token-info)
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message)
       (spanning-info))
    (while (and tlist (not done))
      (setq token-info (car tlist))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (if (not first-spoint) ;; First item in list
          (setq first-spoint spoint) ;; Remember start point
        (if (< spoint first-spoint)
            (progn ;; This is a reversed list
              (when (< epoint here-point)
                (setq done t)))
          (when (> spoint here-point) ;; This is a forward list
            (setq done t))))
      (when (not done)
        (when (and (>= here-point spoint) (<= here-point epoint))
          (setq spanning-info token-info))
        (setq tlist (cdr tlist))))
    (when spanning-info
      (setq ret-val (cons spanning-info tlist)))
    ret-val
    ))


;;
;;  shu-cpp-tokenize-region-for-command
;;
(defun shu-cpp-tokenize-region-for-command (start end &optional limit)
  "Tokenize the region between START and END into a list of all of the C++
tokens contained therein, displaying any error message, if there is one.  If no
error, return the token list, else return nil"
  (let
      ((func 'shu-cpp-tokenize-region)
       (token-list))
    (setq token-list (shu-cpp-token-internal-tokenize-region-for-command func start end limit))
    token-list
    ))


;;
;;  shu-cpp-reverse-tokenize-region-for-command
;;
(defun shu-cpp-reverse-tokenize-region-for-command (start end &optional limit)
  "Reverse tokenize the region between START and END into a list of all of the C++
tokens contained therein, displaying any error message, if there is one.  If no
error, return the token list, else return nil"
  (let
      ((func 'shu-cpp-reverse-tokenize-region)
       (token-list))
    (setq token-list (shu-cpp-token-internal-tokenize-region-for-command func start end limit))
    token-list
    ))


;;
;;  shu-cpp-token-internal-tokenize-region-for-command
;;
(defun shu-cpp-token-internal-tokenize-region-for-command (func start end &optional limit)
  "Internal function to do a forward or reverse parse of the region between START
and END.  FUNC holds the function to be invoked to do the parse.  This would be
either shu-cpp-tokenize-region or shu-cpp-reverse-tokenize-region.  Once the
parse is complete, we check to see if an error was detected.  If an error was
detected we go to the error point and show the error.  Then we return nil to the
caller.  If no error was detected, we return the token-list to the caller.  This
is a convenient way for a command to get the token-list and not have to do anything
to display an error message if an error is encountered.  The command calls this
function and simply exits if nil is returned, knowing that the error message has
already been displayed."
  (let
      ((token-list)
       (tlist)
       (ret-val)
       (error-token-info)
       (token)
       (token-type)
       (error-message)
       (spoint)
       (epoint)
       (emsg)
       (tbuf      (get-buffer-create shu-unit-test-buffer)))
    (setq ret-val (funcall func start end limit))
    (setq error-token-info (car ret-val))
    (setq token-list (cdr ret-val))
    (setq tlist token-list)
    (when error-token-info
      (shu-cpp-token-show-token-info error-token-info)
      (shu-cpp-token-extract-info error-token-info token token-type spoint epoint error-message)
      (setq emsg error-message)
      (when (not error-message)
        (setq emsg "Uknown error at point"))
      (goto-char spoint)
      (message "%s" emsg)
      (setq tlist nil))
    tlist
    ))


;;
;;  shu-cpp-tokenize-region
;;
(defun shu-cpp-tokenize-region (start end &optional limit)
  "Scan the region between START and AND to build a list of tokens that represent the C++
code in the region.  Return a cons cell with two items in it.  The car of the cons cell
is a token-info that represents a parse error.  The cdr of the cons cell is the list of
tokens.  This list is incomplete if the car of the cons cell is not nil.  The optional
LIMIT argument is used to bound the scan as follows.  When we have added to the list the
first token that is beyond the point specified by LIMIT, we stop the scan."
  (let
      ((ret-val)
       (error-token-info)
       (token-list))
    (setq ret-val (shu-cpp-reverse-tokenize-region start end limit))
    (setq token-list (cdr ret-val))
    (setq error-token-info (car ret-val))
    (setq token-list (nreverse token-list))
    (setq ret-val (cons error-token-info token-list))
    ret-val
    ))

;;
;;  shu-cpp-reverse-tokenize-region
;;
(defun shu-cpp-reverse-tokenize-region (start end &optional limit)
  "Scan the region between START and AND to build a list of tokens that represent the C++
code in the region.  Return a cons cell with two items in it.  The car of the cons cell
is a token-info that represents a parse error.  The cdr of the cons cell is the list of
tokens.  This list is incomplete if the car of the cons sell is not nil.  The optional
LIMIT argument is used to bound the scan as follows.  When we have added to the list the
first token that is beyond the point specified by LIMIT, we stop the scan."
  (let
      ((done)
       (spoint)
       (epoint)
       (point-pair)
       (token-info)
       (token-list)
       (slist)
       (info)
       (ext-info)
       (token-type)
       (token)
       (line-col)
       (nextop)
       (error-message)
       (error-token-info)
       (ret-val)
       (stream-start (regexp-opt (list "<<")))
       (string-start (regexp-opt (list "'" "\"") nil)))
    (setq shu-cpp-keywords-hash (shu-get-cpp-keywords-hash))
    (save-excursion
      (save-match-data
        (save-restriction
          (widen)
          (goto-char start)
          (while (not done)
            (skip-chars-forward shu-all-whitespace-regexp-scf end)
            (when (< (point) end)
              (cond
               ((looking-at shu-comment-start-pattern);; Start of a comment
                (setq token-info (shu-cpp-get-comment (point) end)))
               ((looking-at shu-cpp-operators-three)      ;  Three character operator
                (setq token-info (shu-cpp-get-operator-token 3)))
               ((looking-at shu-cpp-operators-two)        ;  Two character operator
                (setq token-info (shu-cpp-get-operator-token 2)))
               ((looking-at shu-cpp-operators-one)        ;  << operator
                (setq token-info (shu-cpp-get-operator-token 1)))
               ((looking-at string-start)       ;; Quoted string
                (setq token-info (shu-cpp-get-quoted-token (point) end)))
               (t                               ;; Unquoted token
                (setq token-info (shu-cpp-get-unquoted-token (point) end))))
              (when token-info
                (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
                (setq token-list (cons token-info token-list))
                (when limit
                  (when (> spoint limit)
                    (setq done t)))
                (when error-message
                  (setq error-token-info token-info)
                  (setq done t))))
            (when (>= (point) end)
              (setq done t))
            ))))
    (setq ret-val (cons error-token-info token-list))
    ret-val
    ))


;;
;;  shu-cpp-get-operator-token
;;
(defun shu-cpp-get-operator-token (length)
  "Fetch the C++ operator that starts at point.  LENGTH is the number of characters
in the operator, which is either 1, 2, or 3."
  (let
      ((op)
       (start (point))
       (token)
       (token-info)
       (token-type shu-cpp-token-type-op))
    (setq token (buffer-substring-no-properties (point) (+ length (point))))
    (forward-char length)
    (setq token-info (shu-cpp-make-token-info token token-type start (1- (point))))
    token-info
    ))



;;
;;  shu-cpp-get-unquoted-token
;;
(defun shu-cpp-get-unquoted-token (start end)
  "Find the unquoted token in the buffer that starts at point.  The token is
terminated either by the position of END or by the regular expression that
defines the end of an unquoted token."
  (let
      ((spoint (point))
       (epoint (1+ start))
       (token)
       (token-type shu-cpp-token-type-uq)
       (token-info)
       (info))
    (when (> (- end start) 0)
      (re-search-forward shu-cpp-token-delimiter-end end 1)
      (forward-char -1)
      (if (looking-at shu-cpp-token-delimiter-end)
          (setq epoint (point))
        (forward-char 1)
        (setq epoint (point))))
    (setq token (buffer-substring-no-properties spoint epoint))
    (when (gethash token shu-cpp-keywords-hash)
      (setq token-type shu-cpp-token-type-kw))
    (setq token-info (shu-cpp-make-token-info token token-type spoint (1- epoint)))
    token-info
    ))


;;
;;  shu-cpp-get-quoted-token
;;
(defun shu-cpp-get-quoted-token (start end)
  "Find the token in the buffer between START and END that is terminated by an
unescaped quote.  On entry, point must be positioned on the quote that starts
the string.  The appropriate error message is returned if there is no unescaped
quote before the end of the current line.  If the character under point is not a
quote start character, nil is returned."
  (let
      ((string-start (regexp-opt (list "'" "\"") nil))
       (quote-char)
       (spoint (point))
       (epoint (1+ start))
       (token)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token-type shu-cpp-token-type-qt)
       (token-info)
       (error-message)
       (quote-close)
       (eol (save-excursion (end-of-line) (point)))
       (info))
    (when (> (- end start) 0)
      (setq quote-char (buffer-substring-no-properties (point) (1+ (point))))
      (when (string= quote-char "'")
        (setq quote-close "[^\\]'"))
      (when (string= quote-char "\"")
        (setq quote-close "[^\\]\""))
      (when quote-close
        (if (not (re-search-forward quote-close eol t))
            (setq error-message (concat "String delimited by " quote-char " is unterminated."))
          (setq epoint (point))
          (setq token (buffer-substring-no-properties spoint epoint)))
        (setq token-info (shu-cpp-make-token-info token token-type spoint (1- epoint) error-message))))
    token-info
    ))


;;
;;  shu-cpp-get-comment
;;
(defun shu-cpp-get-comment (start end)
  "Get the comment that starts at point.  If it starts with //, get to end of
line.  If it starts with /*, skip to terminating */.  If there is no terminating
*/ in the region, create a TOKEN-INFO with the appropriate error message in it."
  (let
      ((old-style (regexp-opt (list "/*") nil))
       (comment-term (regexp-opt (list "*/") nil))
       (new-style (regexp-opt (list "//") nil))
       (eol (save-excursion (end-of-line) (point)))
       (spoint (point))
       (epoint (1+ (point)))
       (token "//")
       (token-type shu-cpp-token-type-ct)
       (token-info)
       (error-message)
       (quote-close)
       (info))
    (if (< (- end start) 2)
        (goto-char end)
      (if (looking-at new-style)
          (progn
            (setq epoint (min end (1+ eol)))
            (setq token (buffer-substring-no-properties spoint epoint))
            (goto-char eol))
        (setq token "/*")
        (if (re-search-forward comment-term end t)
            (progn
              (setq token (buffer-substring-no-properties spoint (point)))
              (setq epoint (point)))
          (setq error-message "Comment started by /* is unterminated")
          (setq epoint (point))
          )
        )
      (setq token-info (shu-cpp-make-token-info token token-type spoint (1- epoint) error-message))
      )
    token-info
    ))


;;
;;  shu-cpp-make-token-info
;;
(defun shu-cpp-make-token-info (token token-type spoint epoint &optional error-message)
  "Pack the supplied arguments into a TOKEN-INFO and return the TOKEN-INFO."
  (let
      ((token-info)
       (info)
       (ext-info)
       (point-pair))
    (setq point-pair (cons spoint epoint))
    (setq ext-info (cons error-message point-pair))
    (setq info (cons token-type ext-info))
    (setq token-info (cons info token))
    token-info
    ))


;;
;;  shu-cpp-replace-token-info
;;
(defun shu-cpp-replace-token-info (token-info token token-type spoint epoint &optional error-message)
  "Replace the supplied arguments in the given TOKEN-INFO and return the TOKEN-INFO."
  (let
      ((info)
       (ext-info)
       (point-pair))
    (setq point-pair (cons spoint epoint))
    (setq ext-info (cons error-message point-pair))
    (setq info (cons token-type ext-info))
    (setq token-info (cons info token))
    token-info
    ))


;;
;;  shu-cpp-copy-token-info
;;
(defun shu-cpp-copy-token-info (token-info)
  "Return a deep copy of the given TOKEN-INFO."
  (let
      ((new-token-info)
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
    (setq new-token-info (shu-cpp-make-token-info token token-type spoint epoint error-message))
    new-token-info
    ))


;;
;;  shu-cpp-compare-token-info
;;
(defun shu-cpp-compare-token-info (token-info1 token-info2)
  "Compare the two instances of TOKEN-INFO, returning true if their contents
are the same."
  (let ((token1)
        (token-type1)
        (spoint1)
        (epoint1)
        (error-message1)
        (token2)
        (token-type2)
        (spoint2)
        (epoint2)
        (error-message2)
        (same))
    (shu-cpp-token-extract-info token-info1 token1 token-type1 spoint1 epoint1 error-message1)
    (shu-cpp-token-extract-info token-info2 token2 token-type2 spoint2 epoint2 error-message2)
    (when (and (string= token1 token2)
               (eq token-type1 token-type2)
               (eq spoint1 spoint2)
               (eq epoint1 epoint2))
      (if error-message1
          (progn
            (when error-message2
              (when (string= error-message1 error-message2)
                (setq same t))))
        (when (not error-message2)
          (setq same t))))
    same
    ))


;;
;;  shu-cpp-compare-token-info-sans-pos
;;
(defun shu-cpp-compare-token-info-sans-pos (token-info1 token-info2)
  "Compare the two instances of TOKEN-INFO, returning true if their contents
are the same.  Do not include the start or end points in the comparison."
  (let ((token1)
        (token-type1)
        (spoint1)
        (epoint1)
        (error-message1)
        (token2)
        (token-type2)
        (spoint2)
        (epoint2)
        (error-message2)
        (same))
    (shu-cpp-token-extract-info token-info1 token1 token-type1 spoint1 epoint1 error-message1)
    (shu-cpp-token-extract-info token-info2 token2 token-type2 spoint2 epoint2 error-message2)
    (when (and (string= token1 token2)
               (eq token-type1 token-type2))
      (if error-message1
          (progn
            (when error-message2
              (when (string= error-message1 error-message2)
                (setq same t))))
        (when (not error-message2)
          (setq same t))))
    same
    ))



;;
;;  shu-cpp-token-info-replace-token
;;
(defun shu-cpp-token-info-replace-token (token-info new-token)
  "Replace the TOKEN of TOKEN-INFO with NEW-TOKEN, returning the
modified TOKEN-INFO"
  (setcdr token-info new-token)
  new-token
  )



;;
;;  shu-cpp-token-info-replace-epoint
;;
(defun shu-cpp-token-info-replace-epoint (token-info new-epoint)
  "Replace the EPOINT of TOKEN-INFO with NEW-EPOINT"
  (let ((info)
        (ext-info)
        (point-pair))
    (setq info (car token-info))
    (setq ext-info (cdr info))
    (setq point-pair (cdr ext-info))
    (setcdr point-pair new-epoint)
    ))




;;
;;  shu-cpp-is-reverse-token-list-balanced
;;
(defun shu-cpp-is-reverse-token-list-balanced (token-list open-char close-char)
  "Return t if a token-list contains matched pairs of OPEN-CHAR and CLOSE-CHAR.
If imbalance is present, print error message and return nil.  Typically OPEN-CHAR
might be a left parenthesis and CLOSE-CHAR might be a right parenthesis.  Or they
might be \"<\" and \">\", or any other pair types.  Note that this function
returns t if there are no occurrences of OPEN-CHAR and CLOSE-CHAR"
  (let (
        (tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (ppoint1)
        (epoint1)
        (balanced t)
        (error-message)
        (plevel 0)
        )
    (setq tlist token-list)
    (setq token-info (car tlist))
    (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
    (when (and (= token-type shu-cpp-token-type-op)
               (string= token ";")) ;;; Ignore trailing semi-colon
      (setq tlist (cdr tlist)))
    (while (and tlist (not epoint1))
      (setq token-info (car tlist))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (when (and (= token-type shu-cpp-token-type-op)
                 (string= token close-char))
        (setq plevel (1+ plevel))
        (when (= plevel 1)
          (setq ppoint1 spoint)))
      (when (and (= token-type shu-cpp-token-type-op)
                 (string= token open-char))
        (setq plevel (1- plevel))
        (when (= plevel -1)
          (setq epoint1 spoint)))
      (setq tlist (cdr tlist)))
    (when (or epoint1 (/= plevel 0))
      (setq balanced nil)
      (ding)
      (if epoint1
          (progn
            (goto-char epoint1)
            (message "%s%s%s%s%s" "\"" open-char "\" with no matching \"" close-char "\""))
        (goto-char ppoint1)
        (message "%s%s%s%s%s" "\"" close-char "\" with no matching \"" open-char "\"")))
    balanced
    ))




;;
;;  shu-cpp-remove-template-parameters
;;
(defun shu-cpp-remove-template-parameters (token-list &optional preserve-template)
  "Remove from the token-list any template parameters (anything between \">\"
and its matching \">\").  In addition, adjust the end point of the token
immediately prior to the template parameter to be that of the endpoint of the
template parameter.
Thus something like the following:
    Mumble<int, double>
becomes the token \"Mumble\" with a length of 19.  If PRESERVE-TEMPLATE is true,
then we change the token that contains the type name by copying the template
parameters into it.  If the type name token was \"Mumble\", then the token
itself is changed to \"Mumble<int, double>\".  The tokens that represent the
template parameters are removed from the token list in either case.
This eliminates any comma that does not immediately follow a parameter name.
As we scan the reverse ordered token list, any comma that we find immediately
precedes a variable name in the parameter list.  There may be intervening
operators and comments.  But once we find a comma, the next unquoted token is
the variable name."
  (let ((debug-on-error t)
        (tlist token-list)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        (plevel 0)
        (prior-tlist)
        (prior-to-close-tlist)
        (adjust-next-epoint)
        (adjusted-epoint))
    (while tlist
      (setq token-info (car tlist))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (when adjust-next-epoint
        (shu-cpp-token-info-replace-epoint token-info adjusted-epoint)
        (setq epoint adjusted-epoint)
        (when preserve-template
          (setq token (buffer-substring-no-properties spoint (1+ epoint)))
          (setq token-info (shu-cpp-token-info-replace-token token-info token)))
        (setq adjust-next-epoint nil))
      (when (and (= token-type shu-cpp-token-type-op)
                 (string= token ">"))
        (setq plevel (1+ plevel))
        (when (eq plevel 1)
          (setq adjusted-epoint epoint)
          (setq prior-to-close-tlist prior-tlist)
          )
        )
      (when (and (= token-type shu-cpp-token-type-op)
                 (string= token "<"))
        (setq plevel (1- plevel))
        (when (eq plevel 0)
          (setcdr prior-to-close-tlist (cdr tlist))
          (setq adjust-next-epoint t)))
      (setq prior-tlist tlist)
      (setq tlist (cdr tlist)))
    ))




;;
;;  shu-cpp-adjust-template-parameters
;;
(defun shu-cpp-adjust-template-parameters (token-list)
  "Turn each set of template parameters in a reverse parsed list (anything between
\">\" and \"<\" into a separate token of type SHU-CPP-TOKEN-TYPE-TP.  e.g., the
five separate tokens \">\", \"double\", \",\", \"int\", \"<\" will be turned into
one new token of type SHU-CPP-TOKEN-TYPE-TP whose token value is \"int, double\"."
  (let ((tlist token-list)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        (plevel 0)
        (prior-tlist)
        (prior-to-close-tlist)
        (remember-epoint)
        (new-token-info)
        (template-spoint)
        (template-epoint))
    (while tlist
      (setq token-info (car tlist))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (when remember-epoint
        (setq template-epoint epoint)
        (setq remember-epoint nil))
      (setq template-spoint (1+ spoint))
      (when (and (= token-type shu-cpp-token-type-op)
                 (string= token ">"))
        (setq plevel (1+ plevel))
        (when (eq plevel 1)
          (setq remember-epoint t)
          (setq prior-to-close-tlist prior-tlist)))
      (when (and (= token-type shu-cpp-token-type-op)
                 (string= token "<"))
        (setq plevel (1- plevel))
        (when (eq plevel 0)
          (setcdr prior-to-close-tlist tlist)
          (setq token (buffer-substring-no-properties template-spoint (1+ template-epoint)))
          (setq new-token-info
                (shu-cpp-make-token-info token shu-cpp-token-type-tp
                                         template-spoint template-epoint))
          (setcar tlist new-token-info)))
      (setq prior-tlist tlist)
      (setq tlist (cdr tlist)))
    ))



;;
;;  shu-cpp-compare-tlist-sans-comment
;;
(defun shu-cpp-compare-tlist-sans-comment (token-list1 token-list2)
  "Compare the two lists of TOKEN-INFO skipping comments and stopping at the end
of the shortest one.  The purpose of this function is to determine if two bits
of reverse parsed code have the same suffix."
  (let(
       (tlist1 token-list1)
       (tlist2 token-list2)
       (token-info1)
       (token-info2)
       (token1)
       (token-type1)
       (spoint1)
       (epoint1)
       (error-message1)
       (token2)
       (token-type2)
       (spoint2)
       (epoint2)
       (error-message2)
       (continue t)
       (continue1)
       (continue2)
       (same t)
       )
    (while continue
      (setq token-info1 (car tlist1))
      (setq token-info2 (car tlist2))
      (shu-cpp-token-extract-info token-info1 token1 token-type1 spoint1 epoint1 error-message1)
      (shu-cpp-token-extract-info token-info2 token2 token-type2 spoint2 epoint2 error-message2)
      (when (or (eq token-type1 shu-cpp-token-type-ct)
                (eq token-type1 shu-cpp-token-type-cc))
        (setq continue1 t)
        (while continue1
          (setq tlist1 (cdr tlist1))
          (if (not tlist1)
              (setq continue1 nil)
            (setq token-info1 (car tlist1))
            (shu-cpp-token-extract-info token-info1 token1 token-type1 spoint1 epoint1 error-message1)
            (when (and (/= token-type1 shu-cpp-token-type-ct)
                       (/= token-type1 shu-cpp-token-type-cc))
              (setq continue1 nil)))))
      (when tlist1
        (when (or (eq token-type2 shu-cpp-token-type-ct)
                  (eq token-type1 shu-cpp-token-type-cc))
          (setq continue2 t)
          (while continue2
            (setq tlist2 (cdr tlist2))
            (if (not tlist2)
                (setq continue2 nil)
              (setq token-info2 (car tlist2))
              (shu-cpp-token-extract-info token-info2 token2 token-type2 spoint2 epoint2 error-message2)
              (when (and (/= token-type2 shu-cpp-token-type-ct)
                         (/= token-type1 shu-cpp-token-type-cc))
                (setq continue2 nil))))))
      (if (or (not tlist1) (not tlist2))
          (setq continue nil)
        (when (or (not (string= token1 token2))
                  (/= token-type1 token-type2))
          (setq same nil)
          (setq continue nil)))
      (when continue
        (when tlist1
          (setq tlist1 (cdr tlist1))
          (when (and tlist1 tlist2)
            (setq tlist2 (cdr tlist2))))
        (when (or (not tlist1) (not tlist2))
          (setq continue nil)))
      )
    same
    ))



;;
;;  shu-cpp-tokenize-show-list
;;
(defun shu-cpp-tokenize-show-list (token-list)
  (let (
        (tlist token-list)
        (token-info)
        )
    (while tlist
      (setq token-info (car tlist))
      (shu-cpp-token-show-token-info token-info)
      (setq tlist (cdr tlist)))
    ))


;;
;;  shu-cpp-token-show-token-info
;;
(defun shu-cpp-token-show-token-info (token-info)
  "Show the data returned by one of the functions in this file that scans for tokens."
  (let
      ((gb      (get-buffer-create shu-unit-test-buffer))
       (info)
       (token-type)
       (token-type-name)
       (ext-info)
       (error-message)
       (emsg "")
       (point-pair)
       (spoint)
       (epoint)
       (token)
       (tok ""))
    (if (not token-info)
        (princ "shu-cpp-token-show-token-info, token-info is nil\n" gb)
      (if (not (consp token-info))
          (progn (princ "shu-cpp-token-show-token-info, token-info not cons: " gb)
                 (princ token-info gb) (princ "\n" gb))
        (setq info (car token-info))
        (if (not info)
            (princ "shu-test-shu-cpp-show-info, info is nil\n" gb)
          (if (not (consp info))
              (progn (princ "shu-cpp-token-show-token-info, info not cons: " gb)
                     (princ info gb) (princ "\n" gb))
            (setq token-type (car info))
            (if (not token-type)
                (princ "shu-test-shu-cpp-show-info, token-type is nil\n" gb)
              (if (not (numberp token-type))
                  (progn (princ "shu-cpp-token-show-token-info, token-type not number: " gb)
                         (princ token-type gb) (princ "\n" gb))
                (setq token-type-name (shu-cpp-token-token-type-name token-type))
                (setq ext-info (cdr info))
                (if (not ext-info)
                    (princ "shu-test-shu-cpp-show-info, ext-info is nil\n" gb)
                  (if (not (consp ext-info))
                      (progn (princ "shu-cpp-token-show-token-info, ext-info not cons: " gb)
                             (princ ext-info gb) (princ "\n" gb))
                    (setq error-message (car ext-info))
                    (setq point-pair (cdr ext-info))
                    (if (not point-pair)
                        (princ "shu-cpp-token-show-token-info, point-pair is nil\n" gb)
                      (if (not (consp point-pair))
                          (progn (princ "shu-cpp-token-show-token-info, point-pair not cons: " gb)
                                 (princ point-pair gb) (princ "\n" gb))
                        (setq spoint (car point-pair))
                        (setq epoint (cdr point-pair))
                        (if (not (numberp spoint))
                            (progn (princ "shu-cpp-token-show-token-info, spoint not number: " gb)
                                   (princ spoint gb) (princ "\n" gb))
                          (if (not (numberp epoint))
                              (progn (princ "shu-cpp-token-show-token-info, epoint not number: " gb)
                                     (princ epoint gb) (princ "\n" gb))
                            (when error-message
                              (setq emsg (concat ", Error = \"" error-message "\".")))
                            (setq token (cdr token-info))
                            (when token
                              (setq tok token))
                            (princ (format "%d : %d = [%s](%d (%s))%s\n" spoint epoint tok token-type token-type-name emsg) gb)))))))))))))
    ))



;;
;;  shu-cpp-token-string-token-info
;;
(defun shu-cpp-token-string-token-info (token-info)
  "Return a string that represents the contentw of the toke-info."
  (interactive)
  (let ((token)
        (token-type)
        (token-type-name)
        (spoint)
        (epoint)
        (error-message)
        (emsg "")
        (name))
    (if (not token-info)
        (setq name "**none**")
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (setq token-type-name (shu-cpp-token-token-type-name token-type))
      (when error-message
        (setq emsg (concat " (" error-message ")")))
      (setq name (format "(%s) \"%s\" %d  %s" token-type-name token spoint emsg)))
    name
    ))



;;
;;  shu-cpp-token-set-alias
;;
(defun shu-cpp-token-set-alias ()
  "Set the common alias names for the functions in shu-cpp-token.
These are usually the same as the function names with the leading
shu- prefix removed."
  (defalias 'parse-region 'shu-cpp-parse-region)
  (defalias 'reverse-parse-region 'shu-cpp-reverse-parse-region)
  )

;;; shu-cpp-token.el ends here
