;;; shu-cpp-token.t.el --- Shu project code unit tests
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Author: Stewart L. Pslmer <stewart@stewartpalmer.com>
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
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

(require 'ert)
(require 'shu-cpp-token)

;;; Code



;;
;;  shu-test-shu-cpp-make-token-info-1
;;
(ert-deftest shu-test-shu-cpp-make-token-info-1 ()
  (let (
        (token "TOKEN")
        (token-type shu-cpp-token-type-qt)
        (spoint 123)
        (epoint 456)
        (error-message "This is an error message")
        (token-info)
        (token2)
        (token-type2)
        (spoint2)
        (epoint2)
        (error-message2)
        (token3)
        (token-type3)
        (spoint3)
        (epoint3)
        (error-message3)
        )
    (setq token-info (shu-cpp-make-token-info token token-type spoint epoint error-message))
    (shu-cpp-token-extract-info token-info token2 token-type2 spoint2 epoint2 error-message2)
    (should token2)
    (should (stringp token2))
    (should (string= token token2))
    (should token-type2)
    (should (numberp token-type2))
    (should (= token-type token-type2))
    (should spoint2)
    (should (numberp spoint2))
    (should (= spoint spoint2))
    (should epoint2)
    (should (numberp epoint2))
    (should (= epoint epoint2))
    (should error-message2)
    (should (stringp error-message2))
    (should (string= error-message error-message2))
    (setq token-type3 (shu-cpp-token-extract-type token-info))
    (should token-type3)
    (should (numberp token-type3))
    (should (= token-type token-type3))
    (setq token3 (shu-cpp-token-extract-token token-info))
    (should token3)
    (should (stringp token3))
    (should (string= token token3))
    (setq spoint3 (shu-cpp-token-extract-spoint token-info))
    (should spoint3)
    (should (numberp spoint3))
    (should (= spoint spoint3))
    (setq epoint3 (shu-cpp-token-extract-epoint token-info))
    (should epoint3)
    (should (numberp epoint3))
    (should (= epoint epoint3))
    ))


;;
;;  shu-test-shu-cpp-token-token-type-name
;;
(ert-deftest shu-test-shu-cpp-token-token-type-name ()
  "Test turning a token type into a token type name."
  (let ((x))
    (setq x (shu-cpp-token-token-type-name shu-cpp-token-type-op))
    (should (string= "operator" x))

    (setq x (shu-cpp-token-token-type-name shu-cpp-token-type-qt))
    (should (string= "quoted-string" x))

    (setq x (shu-cpp-token-token-type-name shu-cpp-token-type-uq))
    (should (string= "unquoted token" x))

    (setq x (shu-cpp-token-token-type-name shu-cpp-token-type-kw))
    (should (string= "key word" x))

    (setq x (shu-cpp-token-token-type-name shu-cpp-token-type-ct))
    (should (string= "comment" x))

    (setq x (shu-cpp-token-token-type-name shu-cpp-token-type-cc))
    (should (string= "code comment" x))

    (setq x (shu-cpp-token-token-type-name shu-cpp-token-type-tp))
    (should (string= "template parameter" x))

    (setq x (shu-cpp-token-token-type-name 93))
    (should (string= "**unknown** (93)" x))

    (setq x (shu-cpp-token-token-type-name "fred"))
    (should (string= "**unknown** (Not a number)" x))

    (setq x (shu-cpp-token-token-type-name nil))
    (should (string= "**unknown** (nil)" x))
    ))


;;
;;  shu-test-shu-cpp-get-unquoted-token
;;
(ert-deftest shu-test-shu-cpp-get-unquoted-token ()
  "Test fetching an unquoted token."
  (let
      ((token-info))

    (with-temp-buffer
      (insert "a")
      (goto-char 1)
      (setq token-info (shu-cpp-get-unquoted-token 1 1))
      (should (shu-test-check-shu-cpp-get-token "unquote-1" token-info "a" shu-cpp-token-type-uq 1 1)))

    (with-temp-buffer
      (insert "abcd ")
      (goto-char 1)
      (setq token-info (shu-cpp-get-unquoted-token (point-min) (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-2" token-info "abcd" shu-cpp-token-type-uq 1 4)))

    (with-temp-buffer
      (insert "abcd<< ")
      (goto-char 1)
      (setq token-info (shu-cpp-get-unquoted-token (point-min) (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-3" token-info "abcd" shu-cpp-token-type-uq 1 4)))

    (with-temp-buffer
      (insert "   abcd")
      (goto-char 4)
      (setq token-info (shu-cpp-get-unquoted-token 4 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-4" token-info "abcd" shu-cpp-token-type-uq 4 7)))

    (with-temp-buffer
      (insert "  ghijkl  ")
      (goto-char 3)
      (setq token-info (shu-cpp-get-unquoted-token 3 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-5" token-info "ghijkl" shu-cpp-token-type-uq 3 8)))

    (with-temp-buffer
      (insert "  std::cout\"  ")
      (goto-char 3)
      (setq token-info (shu-cpp-get-unquoted-token 3 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-6" token-info "std" shu-cpp-token-type-uq 3 5)))

    (with-temp-buffer
      (insert "  std::cout'  ")
      (goto-char 3)
      (setq token-info (shu-cpp-get-unquoted-token 3 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-7" token-info "std" shu-cpp-token-type-uq 3 5)))

    (with-temp-buffer
      (insert "  (x)[12]")
      (goto-char 3)

      (should (not (looking-at shu-cpp-operators-three)))
      (should (not (looking-at shu-cpp-operators-two)))
      (should (looking-at shu-cpp-operators-one))
      (setq token-info (shu-cpp-get-operator-token 1))
      (should (shu-test-check-shu-cpp-get-token "unquote-8" token-info "(" shu-cpp-token-type-op 3 3))

      (setq token-info (shu-cpp-get-unquoted-token (point) (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-9" token-info "x" shu-cpp-token-type-uq 4 4))

      (should (not (looking-at shu-cpp-operators-three)))
      (should (not (looking-at shu-cpp-operators-two)))
      (should (looking-at shu-cpp-operators-one))
      (setq token-info (shu-cpp-get-operator-token 1))
      (should (shu-test-check-shu-cpp-get-token "unquote-10" token-info ")" shu-cpp-token-type-op 5 5))

      (should (not (looking-at shu-cpp-operators-three)))
      (should (not (looking-at shu-cpp-operators-two)))
      (should (looking-at shu-cpp-operators-one))
      (setq token-info (shu-cpp-get-operator-token 1))
      (should (shu-test-check-shu-cpp-get-token "unquote-11" token-info "[" shu-cpp-token-type-op 6 6))

      (setq token-info (shu-cpp-get-unquoted-token (point) (point-max)))
      (should (shu-test-check-shu-cpp-get-token "unquote-12" token-info "12" shu-cpp-token-type-uq 7 8))

      (should (not (looking-at shu-cpp-operators-three)))
      (should (not (looking-at shu-cpp-operators-two)))
      (should (looking-at shu-cpp-operators-one))
      (setq token-info (shu-cpp-get-operator-token 1))
      (should (shu-test-check-shu-cpp-get-token "unquote-13" token-info "]" shu-cpp-token-type-op 9 9))
      )
    ))




;;
;;  shu-test-shu-cpp-get-quoted-token
;;
(ert-deftest shu-test-shu-cpp-get-quoted-token ()
  "Test fetching a quoted token."
  (let
      ((token-info))

    (with-temp-buffer
      (insert "a")
      (setq token-info (shu-cpp-get-quoted-token 1 1))
      (should (not token-info)))

    (with-temp-buffer
      (insert "\"abc\" ")
      (goto-char (point-min))
      (setq token-info (shu-cpp-get-quoted-token 1 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "quote-1" token-info "\"abc\"" shu-cpp-token-type-qt 1 5)))

    (with-temp-buffer
      (insert "\"abc\"")
      (goto-char (point-min))
      (setq token-info (shu-cpp-get-quoted-token 1 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "quote-2" token-info "\"abc\"" shu-cpp-token-type-qt 1 5)))

    (with-temp-buffer
      (insert "\"abc ")
      (goto-char (point-min))
      (setq token-info (shu-cpp-get-quoted-token 1 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "quote-3" token-info nil shu-cpp-token-type-qt 1 1 "String delimited by \" is unterminated.")))

    (with-temp-buffer
      (insert "    \"abc ")
      (goto-char 5)
      (setq token-info (shu-cpp-get-quoted-token 3 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "quote-4" token-info nil shu-cpp-token-type-qt 5 3 "String delimited by \" is unterminated.")))

    (with-temp-buffer
      (insert "'abc' ")
      (goto-char (point-min))
      (setq token-info (shu-cpp-get-quoted-token 1 (point-max)))
      (should (shu-test-check-shu-cpp-get-token "quote-5" token-info "'abc'" shu-cpp-token-type-qt 1 5)))

    (with-temp-buffer
      (insert "'abc ")
      (goto-char (point-min))
      (setq token-info (shu-cpp-get-quoted-token (point-min) (point-max)))
      (should (shu-test-check-shu-cpp-get-token "quote-6" token-info nil shu-cpp-token-type-qt 1 1 "String delimited by ' is unterminated.")))

    (with-temp-buffer
      (insert "'a\\'bc' ")
      (goto-char (point-min))
      (setq token-info (shu-cpp-get-quoted-token (point-min) (point-max)))
      (should (shu-test-check-shu-cpp-get-token "quote-7" token-info "'a\\'bc'" shu-cpp-token-type-qt 1 7)))
    ))



;;
;;  shu-test-shu-cpp-get-comment
;;
(ert-deftest shu-test-shu-cpp-get-comment ()
  "Test fetching a comment."
  (let
      ((token-info))

    (with-temp-buffer
      (insert " //  ")
      (goto-char 2)
      (setq token-info (shu-cpp-get-comment (point) (point-max)))
      (should (= 6 (point)))
      (should (shu-test-check-shu-cpp-get-token "comment-1" token-info "//  " shu-cpp-token-type-ct 2 5)))

    (with-temp-buffer
      (insert " //")
      (goto-char 2)
      (setq token-info (shu-cpp-get-comment (point) (point-max)))
      (should (= 4 (point)))
      (should (shu-test-check-shu-cpp-get-token "comment-2" token-info "//" shu-cpp-token-type-ct 2 3)))

    (with-temp-buffer
      (insert " /*      */  ")
      123456789012
      (goto-char 2)
      (setq token-info (shu-cpp-get-comment (point) (point-max)))
      (should (= 12 (point)))
      (should (shu-test-check-shu-cpp-get-token "comment-3" token-info "/*      */" shu-cpp-token-type-ct 2 11)))

    (with-temp-buffer
      (insert " /*      */")
      123456789012
      (goto-char 2)
      (setq token-info (shu-cpp-get-comment (point) (point-max)))
      (should (= 12 (point)))
      (should (shu-test-check-shu-cpp-get-token "comment-4" token-info "/*      */" shu-cpp-token-type-ct 2 11)))

    (with-temp-buffer
      (insert " /*  \n\n    */  ")
      123456789012
      (goto-char 2)
      (setq token-info (shu-cpp-get-comment (point) (point-max)))
      (should (= 14 (point)))
      (should (shu-test-check-shu-cpp-get-token "comment-5" token-info "/*  \n\n    */" shu-cpp-token-type-ct 2 13)))

    (with-temp-buffer
      (insert " /*  \n\n      ")
      123456789012
      (goto-char 2)
      (setq token-info (shu-cpp-get-comment (point) (point-max)))
      (should (= 2 (point)))
      (should (shu-test-check-shu-cpp-get-token "comment-6" token-info nil shu-cpp-token-type-ct 2 1 "Comment started by /* is unterminated")))
    ))



;;
;;  shu-test-shu-cpp-token-general
;;
(ert-deftest shu-test-shu-cpp-token-general ()
  "Test PARSE-REGION and TOKENIZE-REGION."
  (let
      ((ret-val)
       (token-list)
       (error-message)
       (token-info)
       (tlist))

    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::cout << \"The sum is: \" this + /* Who? */ that<<std::endl;\n"
        "\n"
        "  const unsigned int  &*(x)[12];"))

      (goto-char (point-min))
      (shu-cpp-parse-region (point-min) (point-max))
      (setq ret-val (shu-cpp-tokenize-region  (point-min) (point-max)))
      (should ret-val)
      (should (consp ret-val))
      (setq error-message (car ret-val))
      (should (not error-message))
      (setq token-list (cdr ret-val))
      (should token-list)
      (should (= 26 (length token-list)))
      (setq tlist token-list)

      ;; 3 : 5 = [std](3)
      ;; 6 : 7 = [::](1)
      ;; 8 : 11 = [cout](3)
      ;; 13 : 14 = [<<](1)
      ;; 16 : 29 = ["The sum is: "](2)
      ;; 31 : 34 = [this](3)
      ;; 36 : 36 = [+](1)
      ;; 38 : 47 = [/* Who? */](4)
      ;; 49 : 52 = [that](3)
      ;; 53 : 54 = [<<](1)
      ;; 55 : 57 = [std](3)
      ;; 58 : 59 = [::](1)
      ;; 60 : 63 = [endl](3)
      ;; 64 : 64 = [;](1)
      ;; 69 : 73 = [const](3)
      ;; 75 : 82 = [unsigned](3)
      ;; 84 : 86 = [int](3)
      ;; 89 : 89 = [&](1)
      ;; 90 : 90 = [*](1)
      ;; 91 : 91 = [(](1)
      ;; 92 : 92 = [x](3)
      ;; 93 : 93 = [)](1)
      ;; 94 : 94 = [[](1)
      ;; 95 : 96 = [12](3)
      ;; 97 : 97 = []](1)
      ;; 98 : 98 = [;](1)

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-1" token-info "std" shu-cpp-token-type-uq 3 5))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-2" token-info "::" shu-cpp-token-type-op 6 7))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-3" token-info "cout" shu-cpp-token-type-uq 8 11))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-4" token-info "<<" shu-cpp-token-type-op 13 14))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-5" token-info "\"The sum is: \"" shu-cpp-token-type-qt 16 29))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-6" token-info "this" shu-cpp-token-type-kw 31 34))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-7" token-info "+" shu-cpp-token-type-op 36 36))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-8" token-info "/* Who? */" shu-cpp-token-type-ct 38 47))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-9" token-info "that" shu-cpp-token-type-uq 49 52))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-10" token-info "<<" shu-cpp-token-type-op 53 54))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-11" token-info "std" shu-cpp-token-type-uq 55 57))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-12" token-info "::" shu-cpp-token-type-op 58 59))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-13" token-info "endl" shu-cpp-token-type-uq 60 63))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-14" token-info ";" shu-cpp-token-type-op 64 64))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-15" token-info "const" shu-cpp-token-type-kw 69 73))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-16" token-info "unsigned" shu-cpp-token-type-kw 75 82))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-17" token-info "int" shu-cpp-token-type-kw 84 86))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-18" token-info "&" shu-cpp-token-type-op 89 89))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-19" token-info "*" shu-cpp-token-type-op 90 90))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-20" token-info "(" shu-cpp-token-type-op 91 91))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-21" token-info "x" shu-cpp-token-type-uq 92 92))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-22" token-info ")" shu-cpp-token-type-op 93 93))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-23" token-info "[" shu-cpp-token-type-op 94 94))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-24" token-info "12" shu-cpp-token-type-uq 95 96))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-25" token-info "]" shu-cpp-token-type-op 97 97))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "general-26" token-info ";" shu-cpp-token-type-op 98 98))
      (setq tlist (cdr tlist))
      )
    ))



;;
;;  shu-test-shu-cpp-token-limit
;;
(ert-deftest shu-test-shu-cpp-token-limit ()
  "Check to make sure that the LIMIT parameter is honored."
  (let
      ((ret-val)
       (token-list)
       (error-message)
       (token-info)
       (tlist))

    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::cout << \"The sum is: \" this + /* Who? */ that<<std::endl;\n"
        "\n"
        "  const unsigned int  &*(x)[12];"))

      (goto-char (point-min))
      (setq ret-val (shu-cpp-reverse-tokenize-region  (point-min) (point-max) 88))
      (should ret-val)
      (should (consp ret-val))
      (setq error-message (car ret-val))
      (should (not error-message))
      (setq token-list (cdr ret-val))
      (should token-list)
      (should (= 18 (length token-list)))
      (setq tlist token-list)
      (shu-cpp-tokenize-show-list token-list)

      ;; 89 : 89 = [&](1)
      ;; 84 : 86 = [int](3)
      ;; 75 : 82 = [unsigned](3)
      ;; 69 : 73 = [const](3)
      ;; 64 : 64 = [;](1)
      ;; 60 : 63 = [endl](3)
      ;; 58 : 59 = [::](1)
      ;; 55 : 57 = [std](3)
      ;; 53 : 54 = [<<](1)
      ;; 49 : 52 = [that](3)
      ;; 38 : 47 = [/* Who? */](4)
      ;; 36 : 36 = [+](1)
      ;; 31 : 34 = [this](3)
      ;; 16 : 29 = ["The sum is: "](2)
      ;; 13 : 14 = [<<](1)
      ;; 8 : 11 = [cout](3)
      ;; 6 : 7 = [::](1)
      ;; 3 : 5 = [std](3)


      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "limit-1" token-info "&" shu-cpp-token-type-op 89 89))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "limit-2" token-info "int" shu-cpp-token-type-kw 84 86))
      (setq tlist (cdr tlist))
      )))



;;
;;  shu-test-shu-cpp-token-limit-forward
;;
(ert-deftest shu-test-shu-cpp-token-limit-forward ()
  "Check to make sure that the LIMIT parameter is honored."
  (let
      ((ret-val)
       (token-list)
       (error-message)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token-info)
       (tlist))

    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::cout << \"The sum is: \" this + /* Who? */ that<<std::endl;\n"
        "\n"
        "  const unsigned int  &*(x)[12];"))

      (goto-char (point-min))
      (princ "shu-test-shu-cpp-token-limit-forward\n" tbuf)
      (setq ret-val (shu-cpp-tokenize-region  (point-min) (point-max) 88))
      (should ret-val)
      (should (consp ret-val))
      (setq error-message (car ret-val))
      (should (not error-message))
      (setq token-list (cdr ret-val))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (should (= 18 (length token-list)))
      (setq tlist token-list)

      ;;  3 :  5 = [std](3)
      ;;  6 :  7 = [::](1)
      ;;  8 : 11 = [cout](3)
      ;; 13 : 14 = [<<](1)
      ;; 16 : 29 = ["The sum is: "](2)
      ;; 31 : 34 = [this](3)
      ;; 36 : 36 = [+](1)
      ;; 38 : 47 = [/* Who? */](4)
      ;; 49 : 52 = [that](3)
      ;; 53 : 54 = [<<](1)
      ;; 55 : 57 = [std](3)
      ;; 58 : 59 = [::](1)
      ;; 60 : 63 = [endl](3)
      ;; 64 : 64 = [;](1)
      ;; 69 : 73 = [const](3)
      ;; 75 : 82 = [unsigned](3)
      ;; 84 : 86 = [int](3)
      ;; 89 : 89 = [&](1)


      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "fwd-limit-1" token-info "std" shu-cpp-token-type-uq 3 5))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "fwd-limit-2" token-info "::" shu-cpp-token-type-op 6 7))
      (setq tlist (cdr tlist))
      )))


;;
;;  shu-test-shu-cpp-token-string-error
;;
(ert-deftest shu-test-shu-cpp-token-string-error ()
  (let
      (
       (ret-val)
       (token-list)
       (error-message)
       (token-info)
       (tlist)
       )


    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::cout << \"The sum is:      \n"
        "\n"
        "  const unsigned int  &*(x)[12];"))

      (goto-char (point-min))
      (shu-cpp-parse-region (point-min) (point-max))
      (setq ret-val (shu-cpp-tokenize-region  (point-min) (point-max)))
      (should ret-val)
      (should (consp ret-val))
      (setq error-message (car ret-val))
      (should error-message)
      (setq token-list (cdr ret-val))
      (should token-list)
      (should (= 5 (length token-list)))
      (setq tlist token-list)

      ;; 3 : 5 = [std](3)
      ;; 6 : 7 = [::](1)
      ;; 8 : 11 = [cout](3)
      ;; 13 : 14 = [<<](1)
      ;; 16 : 16 = [](2), Error = "String delimited by " is unterminated.".

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "string-error-1" token-info "std" shu-cpp-token-type-uq 3 5))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "string-error-2" token-info "::" shu-cpp-token-type-op 6 7))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "string-error-3" token-info "cout" shu-cpp-token-type-uq 8 11))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "string-error-4" token-info "<<" shu-cpp-token-type-op 13 14))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "string-error-5" token-info nil shu-cpp-token-type-qt 16 16 "String delimited by \" is unterminated."))
      (setq tlist (cdr tlist))
      )))



;;
;;  shu-test-shu-cpp-token-comment-error
;;
(ert-deftest shu-test-shu-cpp-token-comment-error ()
  (let
      ((ret-val)
       (token-list)
       (error-message)
       (token-info)
       (tlist))


    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::cout << /*       "
        "\n"
        "  const unsigned int  &*(x)[12];"))

      (goto-char (point-min))
      (shu-cpp-parse-region (point-min) (point-max))
      (setq ret-val (shu-cpp-tokenize-region  (point-min) (point-max)))
      (should ret-val)
      (should (consp ret-val))
      (setq error-message (car ret-val))
      (should error-message)
      (setq token-list (cdr ret-val))
      (should token-list)
      (should (= 5 (length token-list)))
      (setq tlist token-list)


      ;; 3 : 5 = [std](3)
      ;; 6 : 7 = [::](1)
      ;; 8 : 11 = [cout](3)
      ;; 13 : 14 = [<<](1)
      ;; 16 : 15 = [/*](4), Error = "Comment started by /* is unterminated".


      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "comment-error-1" token-info "std" shu-cpp-token-type-uq 3 5))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "comment-error-2" token-info "::" shu-cpp-token-type-op 6 7))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "comment-error-3" token-info "cout" shu-cpp-token-type-uq 8 11))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "comment-error-4" token-info "<<" shu-cpp-token-type-op 13 14))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "comment-error-5" token-info nil shu-cpp-token-type-ct 16 15 "Comment started by /* is unterminated"))
      (setq tlist (cdr tlist))
      )))



;;
;;  shu-test-shu-cpp-reverse-tokenize-region-for-command-1
;;
(ert-deftest shu-test-shu-cpp-reverse-tokenize-region-for-command-1 ()
  (let
      ((ret-val)
       (token-list)
       (error-message)
       (token-info)
       (tlist))

    (with-temp-buffer
      (insert
       (concat
        "class mumble::brumble {\n"
        " thing::foo  (*x)[4];  /* Something */\n"
        " // or other \n"
        " void get_x() const\n"
        " { return x; }\n"))

      ;;      111 : 111 = [}](1)
      ;;      109 : 109 = [;](1)
      ;;      108 : 108 = [x](3)
      ;;      101 : 106 = [return](3)
      ;;      99 : 99 = [{](1)
      ;;      92 : 96 = [const](3)
      ;;      90 : 90 = [)](1)
      ;;      89 : 89 = [(](1)
      ;;      84 : 88 = [get_x](3)
      ;;      79 : 82 = [void](3)
      ;;      65 : 77 = [// or other ](4)
      ;;      48 : 62 = [/* Something */](4)
      ;;      45 : 45 = [;](1)
      ;;      44 : 44 = []](1)
      ;;      43 : 43 = [4](3)
      ;;      42 : 42 = [[](1)
      ;;      41 : 41 = [)](1)
      ;;      40 : 40 = [x](3)
      ;;      39 : 39 = [*](1)
      ;;      38 : 38 = [(](1)
      ;;      33 : 35 = [foo](3)
      ;;      31 : 32 = [::](1)
      ;;      26 : 30 = [thing](3)
      ;;      23 : 23 = [{](1)
      ;;      15 : 21 = [brumble](3)
      ;;      13 : 14 = [::](1)
      ;;      7 : 12 = [mumble](3)
      ;;      1 : 5 = [class](3)
      ;;

      (goto-char (point-max))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command  (point-min) (point-max)))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (should (= 28 (length token-list)))
      (setq tlist token-list)

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-1" token-info "}" shu-cpp-token-type-op 111 111))

      (setq tlist (cdr tlist))
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-2" token-info ";" shu-cpp-token-type-op 109 109))

      (setq tlist (cdr tlist))
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-3" token-info "x" shu-cpp-token-type-uq 108 108))

      (setq tlist (cdr tlist))
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-4" token-info "return" shu-cpp-token-type-kw 101 106))

      (setq tlist (nthcdr 6 tlist))
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-5" token-info "void" shu-cpp-token-type-kw 79 82))

      (setq tlist (cdr tlist))
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-6" token-info "// or other \n" shu-cpp-token-type-ct 65 77))

      (setq tlist (cdr tlist))
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-7" token-info "/* Something */" shu-cpp-token-type-ct 48 62))

      (setq tlist (nthcdr 6 tlist))
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "rcmd-8" token-info "x" shu-cpp-token-type-uq 40 40))
      )))




;;
;;  shu-test-shu-cpp-reverse-tokenize-region-for-command-2
;;
(ert-deftest shu-test-shu-cpp-reverse-tokenize-region-for-command-2 ()
  (let
      ((ret-val)
       (token-list)
       (error-message)
       (token-info)
       (tlist))

    (with-temp-buffer
      (insert
       (concat
        "class mumble::brumble {\n"
        " thing::foo  (*x)[4];  /* Something   \n"
        " // or other \n"
        " void get_x() const\n"
        " { return x; }\n"))


      (goto-char (point-max))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command  (point-min) (point-max)))
      (should (not token-list))
      )))




;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-fwd-1
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-fwd-1 ()
  (let
      ((here-point)
       (token-list)
       (ret-val)
       (token-info)
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::co"))
      (setq here-point (point))
      (insert
       (concat
        "ut << \"The sum is:      \n"
        "\n"
        "  const unsigned int  &*(x)[12];"))
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "fwd-span-1" token-info "cout" shu-cpp-token-type-uq 8 11))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-reverse-1
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-reverse-1 ()
  (let
      ((here-point)
       (token-list)
       (ret-val)
       (token-info)
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::co"))
      (setq here-point (point))
      (insert
       (concat
        "ut << \"The sum is:      \n"
        "\n"
        "  const unsigned int  &*(x)[12];"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "reverse-span-1" token-info "cout" shu-cpp-token-type-uq 8 11))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-fwd-2
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-fwd-2 ()
  (let
      ((here-point)
       (ret-val)
       (token-list)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::cout << \"The sum "))
      (setq here-point (point))
      (insert
       (concat " is:     \"\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-fwd-2\n" tbuf)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "fwd-span-2" token-info "\"The sum  is:     \"" shu-cpp-token-type-qt 16 34))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-reverse-2
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-reverse-2 ()
  (let
      ((here-point)
       (ret-val)
       (token-list)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        " std::cout << \"The sum "))
      (setq here-point (point))
      (insert
       (concat " is:     \"\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-reverse-2\n" tbuf)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "reverse-span-2" token-info "\"The sum  is:     \"" shu-cpp-token-type-qt 16 34))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-fwd-3
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-fwd-3 ()
  (let
      ((here-point)
       (ret-val)
       (token-list)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "/* \"Hell"))
      (setq here-point (point))
      (insert
       (concat "o!\"  */\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-fwd-3\n" tbuf)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "fwd-span-3" token-info "/* \"Hello!\"  */" shu-cpp-token-type-ct 2 16))
      )))



;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-reverse-3
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-reverse-3 ()
  (let
      ((here-point)
       (token-list)
       (ret-val)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "/* \"Hell"))
      (setq here-point (point))
      (insert
       (concat "o!\"  */\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-reverse-3\n" tbuf)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "reverse-span-3" token-info "/* \"Hello!\"  */" shu-cpp-token-type-ct 2 16))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-fwd-4
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-fwd-4 ()
  (let
      ((here-point)
       (token-list)
       (ret-val)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "  int n;\n"
        "// This is a "))
      (setq here-point (point))
      (insert
       (concat "new comment\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-fwd-4\n" tbuf)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "fwd-span-4" token-info "// This is a new comment\n" shu-cpp-token-type-ct 11 35))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-reverse-4
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-reverse-4 ()
  (let
      ((here-point)
       (token-list)
       (ret-val)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "  int n;\n"
        "// This is a "))
      (setq here-point (point))
      (insert
       (concat "new comment\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-reverse-4\n" tbuf)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should ret-val)
      (setq token-info (car ret-val))
      (should (shu-test-check-shu-cpp-get-token "reverse-span-4" token-info "// This is a new comment\n" shu-cpp-token-type-ct 11 35))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-fwd-5
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-fwd-5 ()
  (let
      ((here-point)
       (token-list)
       (ret-val)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "  int n;\n"
        "    int  \n"))
      (setq here-point (point))
      (insert
       (concat "  d_count;\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-fwd-5\n" tbuf)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should (not ret-val))
      )))


;;
;;  shu-test-shu-cpp-token-find-spanning-info-token-reverse-5
;;
(ert-deftest shu-test-shu-cpp-token-find-spanning-info-token-reverse-5 ()
  (let
      ((here-point)
       (token-list)
       (ret-val)
       (token-info)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (token)
       (token-type)
       (spoint)
       (epoint)
       (error-message))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "  int n;\n"
        "    int  \n"))
      (setq here-point (point))
      (insert
       (concat "  d_count;\n"
               "\n"
               "  const unsigned int  &*(x)[12];"))
      (princ "shu-test-shu-cpp-token-find-spanning-info-token-reverse-5\n" tbuf)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max) here-point))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-token-find-spanning-info-token token-list here-point))
      (should (not ret-val))
      )))




;;
;;  shu-test-shu-cpp-reverse-tokenize-region-for-command-all-operators
;;
(ert-deftest shu-test-shu-cpp-reverse-tokenize-region-for-command-all-operators ()
  "Fill a buffer with all 52 operators and parse them all."
  (let
      ((ret-val)
       (token-list)
       (error-message)
       (token-info)
       (tlist))

    (with-temp-buffer
      (insert
       (concat
        "->*" "<<=" ">>="
        "::" "++" "--" "->" "++" "--" ".*" "<<" ">>" "<=" ">=" "=="
        "!=" "&&" "||" "+=" "-=" "*=" "/=" "%=" "&=" "~=" "|="
        "(" ")" "[" "]" "." "+" "-" "!" "~" "*" "&" "*"
        "/" "%" "<" ">" "&" "^" "|" "?" ":" "=" "," ";" "{" "}"))
      (goto-char (point-max))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command  (point-min) (point-max)))
      (should token-list)
      (shu-cpp-tokenize-show-list token-list)
      (should (= 52 (length token-list)))
      (setq tlist token-list)


     ;;;   1   81 : 81 = [}](1)
     ;;;   2   80 : 80 = [{](1)
     ;;;   3   79 : 79 = [;](1)
     ;;;   4   78 : 78 = [,](1)
     ;;;   5   77 : 77 = [=](1)
     ;;;   6   76 : 76 = [:](1)
     ;;;   7   75 : 75 = [?](1)
     ;;;   8   74 : 74 = [|](1)
     ;;;   9   73 : 73 = [^](1)
     ;;;  10   72 : 72 = [&](1)
     ;;;  11   71 : 71 = [>](1)
     ;;;  12   70 : 70 = [<](1)
     ;;;  13   69 : 69 = [%](1)
     ;;;  14   68 : 68 = [/](1)
     ;;;  15   67 : 67 = [*](1)
     ;;;  16   66 : 66 = [&](1)
     ;;;  17   65 : 65 = [*](1)
     ;;;  18   64 : 64 = [~](1)
     ;;;  19   63 : 63 = [!](1)
     ;;;  20   62 : 62 = [-](1)
     ;;;  21   61 : 61 = [+](1)
     ;;;  22   60 : 60 = [.](1)
     ;;;  23   59 : 59 = []](1)
     ;;;  24   58 : 58 = [[](1)
     ;;;  25   57 : 57 = [)](1)
     ;;;  26   56 : 56 = [(](1)
     ;;;  27   54 : 55 = [|=](1)
     ;;;  28   52 : 53 = [~=](1)
     ;;;  29   50 : 51 = [&=](1)
     ;;;  30   48 : 49 = [%=](1)
     ;;;  31   46 : 47 = [/=](1)
     ;;;  32   44 : 45 = [*=](1)
     ;;;  33   42 : 43 = [-=](1)
     ;;;  34   40 : 41 = [+=](1)
     ;;;  35   38 : 39 = [||](1)
     ;;;  36   36 : 37 = [&&](1)
     ;;;  37   34 : 35 = [!=](1)
     ;;;  38   32 : 33 = [==](1)
     ;;;  39   30 : 31 = [>=](1)
     ;;;  40   28 : 29 = [<=](1)
     ;;;  41   26 : 27 = [>>](1)
     ;;;  42   24 : 25 = [<<](1)
     ;;;  43   22 : 23 = [.*](1)
     ;;;  44   20 : 21 = [--](1)
     ;;;  45   18 : 19 = [++](1)
     ;;;  46   16 : 17 = [->](1)
     ;;;  47   14 : 15 = [--](1)
     ;;;  48   12 : 13 = [++](1)
     ;;;  49   10 : 11 = [::](1)
     ;;;  50    7 :  9 = [>>=](1)
     ;;;  51    4 :  6 = [<<=](1)
     ;;;  52    1 :  3 = [->*](1)


      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-1" token-info "}" shu-cpp-token-type-op 81 81))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-2" token-info "{" shu-cpp-token-type-op 80 80))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-3" token-info ";" shu-cpp-token-type-op 79 79))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-4" token-info "," shu-cpp-token-type-op 78 78))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-5" token-info "=" shu-cpp-token-type-op 77 77))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-6" token-info ":" shu-cpp-token-type-op 76 76))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-7" token-info "?" shu-cpp-token-type-op 75 75))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-8" token-info "|" shu-cpp-token-type-op 74 74))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-9" token-info "^" shu-cpp-token-type-op 73 73))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-10" token-info "&" shu-cpp-token-type-op 72 72))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-11" token-info ">" shu-cpp-token-type-op 71 71))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-12" token-info "<" shu-cpp-token-type-op 70 70))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-13" token-info "%" shu-cpp-token-type-op 69 69))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-14" token-info "/" shu-cpp-token-type-op 68 68))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-15" token-info "*" shu-cpp-token-type-op 67 67))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-16" token-info "&" shu-cpp-token-type-op 66 66))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-17" token-info "*" shu-cpp-token-type-op 65 65))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-18" token-info "~" shu-cpp-token-type-op 64 64))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-19" token-info "!" shu-cpp-token-type-op 63 63))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-20" token-info "-" shu-cpp-token-type-op 62 62))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-21" token-info "+" shu-cpp-token-type-op 61 61))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-22" token-info "." shu-cpp-token-type-op 60 60))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-23" token-info "]" shu-cpp-token-type-op 59 59))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-24" token-info "[" shu-cpp-token-type-op 58 58))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-25" token-info ")" shu-cpp-token-type-op 57 57))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-26" token-info "(" shu-cpp-token-type-op 56 56))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-27" token-info "|=" shu-cpp-token-type-op 54 55))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-28" token-info "~=" shu-cpp-token-type-op 52 53))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-29" token-info "&=" shu-cpp-token-type-op 50 51))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-30" token-info "%=" shu-cpp-token-type-op 48 49))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-31" token-info "/=" shu-cpp-token-type-op 46 47))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-32" token-info "*=" shu-cpp-token-type-op 44 45))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-33" token-info "-=" shu-cpp-token-type-op 42 43))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-34" token-info "+=" shu-cpp-token-type-op 40 41))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-35" token-info "||" shu-cpp-token-type-op 38 39))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-36" token-info "&&" shu-cpp-token-type-op 36 37))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-37" token-info "!=" shu-cpp-token-type-op 34 35))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-38" token-info "==" shu-cpp-token-type-op 32 33))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-39" token-info ">=" shu-cpp-token-type-op 30 31))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-40" token-info "<=" shu-cpp-token-type-op 28 29))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-41" token-info ">>" shu-cpp-token-type-op 26 27))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-42" token-info "<<" shu-cpp-token-type-op 24 25))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-43" token-info ".*" shu-cpp-token-type-op 22 23))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-44" token-info "--" shu-cpp-token-type-op 20 21))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-45" token-info "++" shu-cpp-token-type-op 18 19))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-46" token-info "->" shu-cpp-token-type-op 16 17))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-47" token-info "--" shu-cpp-token-type-op 14 15))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-48" token-info "++" shu-cpp-token-type-op 12 13))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-49" token-info "::" shu-cpp-token-type-op 10 11))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-50" token-info ">>=" shu-cpp-token-type-op 7 9))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-51" token-info "<<=" shu-cpp-token-type-op 4 6))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "op-check-52" token-info "->*" shu-cpp-token-type-op 1 3))


      )))



(ert-deftest shu-test-shu-cpp-is-reverse-token-list-balanced-1 ()
  "Test SHU-CPP-IS-REVERSE-TOKEN-LIST-BALANCED with no operators."
  (let ((token-list))
    (with-temp-buffer
      (insert
       (concat
        "  Hi there and happy birthday"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "(" ")"))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">")))
    ))
(ert-deftest shu-test-shu-cpp-is-reverse-token-list-balanced-2 ()
  "Test SHU-CPP-IS-REVERSE-TOKEN-LIST-BALANCED with balanced ()."
  (let ((token-list))
    (with-temp-buffer
      (insert
       (concat
        "  Hi there and (by the way) happy birthday"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "(" ")"))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">")))
    ))
(ert-deftest shu-test-shu-cpp-is-reverse-token-list-balanced-3 ()
  "Test SHU-CPP-IS-REVERSE-TOKEN-LIST-BALANCED with balanced <>."
  (let ((token-list))
    (with-temp-buffer
      (insert
       (concat
        "  Hi there and <by the way> happy birthday"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (shu-cpp-tokenize-show-list token-list)
      (should (shu-cpp-is-reverse-token-list-balanced token-list "(" ")"))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">")))
    ))
(ert-deftest shu-test-shu-cpp-is-reverse-token-list-balanced-4 ()
  "Test SHU-CPP-IS-REVERSE-TOKEN-LIST-BALANCED with balanced () and <>."
  (let ((token-list))
    (with-temp-buffer
      (insert
       (concat
        "  Hi there and (by the way <and forever>) happy birthday"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "(" ")"))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">")))
    ))
(ert-deftest shu-test-shu-cpp-is-reverse-token-list-balanced-5 ()
  "Test SHU-CPP-IS-REVERSE-TOKEN-LIST-BALANCED with balanced () and <>."
  (let ((token-list))
    (with-temp-buffer
      (insert
       (concat
        "  Hi there (and (by the way <and forever>)) < <happy> birthday>"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "(" ")"))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">")))
    ))
(ert-deftest shu-test-shu-cpp-is-reverse-token-list-balanced-6 ()
  "Test SHU-CPP-IS-REVERSE-TOKEN-LIST-BALANCED with unbalanced ()."
  (let ((token-list))
    (with-temp-buffer
      (insert
       (concat
        "  Hi there (and (by the way <and forever>) < <happy> birthday>"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (not (shu-cpp-is-reverse-token-list-balanced token-list "(" ")")))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">")))
    ))
(ert-deftest shu-test-shu-cpp-is-reverse-token-list-balanced-7 ()
  "Test SHU-CPP-IS-REVERSE-TOKEN-LIST-BALANCED with unbalanced ()."
  (let ((token-list))
    (with-temp-buffer
      (insert
       (concat
        "  Hi there (and (by the way <and forever>))) < <happy> birthday>"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (not (shu-cpp-is-reverse-token-list-balanced token-list "(" ")")))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">")))
    ))



;;
;;  shu-test-check-shu-cpp-get-token
;;
(defun shu-test-check-shu-cpp-get-token (test-id token-info xtoken xtoken-type xspoint xepoint &optional xerror-message)
  "Examine the data in TOKEN-INFO to verify that it matches the expectd values specified by
XTOKEN, XTOKEN-TYPE, XSPOINT, and XEPOINT.  If XERROR-MESSAGE is specified, it is compared
to the value of error-message.  Any of the expected values may be specified as nil, in
which case they are not compared to the values derived from TOKEN-INFO.  TEST-ID may
me anything.  It is printed on test failure to identify the test that failed."
  (let
      ((gb      (get-buffer-create shu-unit-test-buffer))
       (info)
       (token-type)
       (ext-info)
       (error-message)
       (emsg "")
       (point-pair)
       (info-pair)
       (nesting-level)
       (spoint)
       (epoint)
       (token)
       (result)
       (tok ""))
    (if (not token-info)
        (princ "shu-test-check-shu-cpp-get-token, token-info is nil\n" gb)
      (if (not (consp token-info))
          (progn (princ "shu-test-check-shu-cpp-get-token, token-info not cons: " gb)
                 (princ token-info gb) (princ "\n" gb))
        (setq info (car token-info))
        (setq token (cdr token-info))
        (if (not info)
            (princ "shu-test-shu-cpp-show-info, info is nil\n" gb)
          (if (not (consp info))
              (progn (princ "shu-test-check-shu-cpp-get-token, info not cons: " gb)
                     (princ info gb) (princ "\n" gb))
            (if (and xtoken (not token))
                (princ (concat "Expected token = [" xtoken "].  Actual token nil") gb)
              (if (and token (not (stringp token)))
                  (progn (princ "Token is not string [" gb) (princ token gb) (princ "]\n" gb))
                (if (and xtoken (not (string= xtoken token)))
                    (princ (concat "Expected token [" xtoken "] not equal actual [" token "]") gb)
                  (setq token-type (car info))
                  (if (not token-type)
                      (princ "shu-test-shu-cpp-show-info, token-type is nil\n" gb)
                    (if (not (numberp token-type))
                        (progn (princ "shu-test-check-shu-cpp-get-token, token-type not number: " gb)
                               (princ token-type gb) (princ "\n" gb))
                      (setq ext-info (cdr info))
                      (if (not ext-info)
                          (princ "shu-test-shu-cpp-show-info, ext-info is nil\n" gb)
                        (if (not (consp ext-info))
                            (progn (princ "shu-test-check-shu-cpp-get-token, ext-info not cons: " gb)
                                   (princ ext-info gb) (princ "\n" gb))
                          (setq point-pair (cdr ext-info))
                          (if (not point-pair)
                              (princ "shu-test-check-shu-cpp-get-token, point-pair is nil\n" gb)
                            (if (not (consp point-pair))
                                (progn (princ "shu-test-check-shu-cpp-get-token, point-pair not cons: " gb)
                                       (princ point-pair gb) (princ "\n" gb))
                              (setq spoint (car point-pair))
                              (setq epoint (cdr point-pair))
                              (if (not (numberp spoint))
                                  (progn (princ "shu-test-check-shu-cpp-get-token, spoint not number: " gb)
                                         (princ spoint gb) (princ "\n" gb))
                                (if (not (numberp epoint))
                                    (progn (princ "shu-test-check-shu-cpp-get-token, epoint not number: " gb)
                                           (princ epoint gb) (princ "\n" gb))
                                  (if (and xtoken-type (/= xtoken-type token-type))
                                      (princ (format "Expected token-type (%d) not = token-type (%d)\n" xtoken-type token-type) gb)
                                    (if (and xspoint (/= xspoint spoint))
                                        (princ (format "Expected spoint (%d) not = spoint (%d)\n" xspoint spoint) gb)
                                      (if (and xepoint (/= xepoint epoint))
                                          (princ (format "Expected epoint (%d) not = epoint (%d)\n" xepoint epoint) gb)
                                        (setq info-pair (car ext-info))
                                        (if (not (consp info-pair))
                                            (progn (princ "shu-test-check-shu-cpp-get-token, info-pair not cons: " gb)
                                                   (princ info-pair gb) (princ "\n" gb))
                                          (setq error-message (car info-pair))
                                          (setq nesting-level (cdr info-pair))
                                          (if (and xerror-message (not error-message))
                                              (princ (concat "Expected error-message = [" xerror-message "].  Actual error-message nil") gb)
                                            (if (and error-message (not (stringp error-message)))
                                                (progn (princ "Error-Message is not string [" gb) (princ error-message gb) (princ "]\n" gb))
                                              (if (and xerror-message (not (string= xerror-message error-message)))
                                                  (princ (concat "Expected error-message [" xerror-message "] not equal actual [" error-message "]") gb)
                                                (when error-message
                                                  (setq emsg (concat ", Error = \"" error-message "\".")))
                                                (when token
                                                  (setq tok token))
                                                (princ "\"" gb) (princ test-id gb) (princ "\": " gb)
                                                (princ (format "%d : %d = [%s](%d)%s\n" spoint epoint tok token-type emsg) gb)
                                                (setq result t)))))))))))))))))))))))
    (when (not result)
      (princ ": shu-test-check-shu-cpp-get-token, test \"" gb)
      (princ test-id gb)
      (princ " \" failed:\n" gb)
      (princ token-info gb)
      (princ "\n" gb))
    result
    ))


;;
;;  shu-this-is-a-function-to-generate-some-test-code
;;
(defun shu-this-is-a-function-to-generate-some-test-code ()
  (interactive)
  (let (
        (count 0)
        (gb      (get-buffer-create shu-unit-test-buffer))
        (ss "\\s-+\\([0-9]+\\)\\s-+:\\s-+\\([0-9]+\\)\\s-+=\\s-+\\[\\([][!%&()*+,-./:<=>?^|~;{}]+\\)\\](1)")
        (spoint)
        (epoint)
        (op-string)
        )
    (with-temp-buffer
      (insert
       (concat
        " 81 : 81 = [}](1)  "
        " 80 : 80 = [{](1)  "
        " 79 : 79 = [;](1)  "
        " 78 : 78 = [,](1)  "
        " 77 : 77 = [=](1)  "
        " 76 : 76 = [:](1)  "
        " 75 : 75 = [?](1)  "
        " 74 : 74 = [|](1)  "
        " 73 : 73 = [^](1)  "
        " 72 : 72 = [&](1)  "
        " 71 : 71 = [>](1)  "
        " 70 : 70 = [<](1)  "
        " 69 : 69 = [%](1)  "
        " 68 : 68 = [/](1)  "
        " 67 : 67 = [*](1)  "
        " 66 : 66 = [&](1)  "
        " 65 : 65 = [*](1)  "
        " 64 : 64 = [~](1)  "
        " 63 : 63 = [!](1)  "
        " 62 : 62 = [-](1)  "
        " 61 : 61 = [+](1)  "
        " 60 : 60 = [.](1)  "
        " 59 : 59 = []](1)  "
        " 58 : 58 = [[](1)  "
        " 57 : 57 = [)](1)  "
        " 56 : 56 = [(](1)  "
        " 54 : 55 = [|=](1) "
        " 52 : 53 = [~=](1) "
        " 50 : 51 = [&=](1) "
        " 48 : 49 = [%=](1) "
        " 46 : 47 = [/=](1) "
        " 44 : 45 = [*=](1) "
        " 42 : 43 = [-=](1) "
        " 40 : 41 = [+=](1) "
        " 38 : 39 = [||](1) "
        " 36 : 37 = [&&](1) "
        " 34 : 35 = [!=](1) "
        " 32 : 33 = [==](1) "
        " 30 : 31 = [>=](1) "
        " 28 : 29 = [<=](1) "
        " 26 : 27 = [>>](1) "
        " 24 : 25 = [<<](1) "
        " 22 : 23 = [.*](1) "
        " 20 : 21 = [--](1) "
        " 18 : 19 = [++](1) "
        " 16 : 17 = [->](1) "
        " 14 : 15 = [--](1) "
        " 12 : 13 = [++](1) "
        " 10 : 11 = [::](1) "
        "  7 :  9 = [>>=](1) "
        "  4 :  6 = [<<=](1) "
        "  1 :  3 = [->*](1) " ))
      (goto-char (point-min))
      (while (re-search-forward ss nil t)
        (setq count (1+ count))
        (princ "\n(setq token-info (car tlist))\n" gb)
        (setq spoint (match-string 1))
        (setq epoint (match-string 2))
        (setq op-string (match-string 3))
        (princ
         (format
          "(should (shu-test-check-shu-cpp-get-token \"op-check-%d\" token-info \"%s\" shu-cpp-token-type-op %s %s))\n"
          count op-string spoint epoint) gb)
        (princ "(setq tlist (cdr tlist))\n" gb))
      )))



;;
;;  shu-test-shu-cpp-remove-template-parameters
;;
(ert-deftest shu-test-shu-cpp-remove-template-parameters ()
  "Test SHU-CPP-REMOVE-TEMPLATE-PARAMETERS"
  (let ((token-list)
        (tlist)
        (token-info))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "(ThingBob  &thingbob,\n"
        " Mumble<int, double>   &mumb1)"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "(" ")"))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">"))
      (shu-cpp-remove-template-parameters token-list t)
      (setq tlist token-list)
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-1" token-info ")" shu-cpp-token-type-op 53 53))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-2" token-info "mumb1" shu-cpp-token-type-uq 48 52))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-3" token-info "&" shu-cpp-token-type-op 47 47))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-4" token-info "Mumble<int, double>" shu-cpp-token-type-uq 25 43))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-5" token-info "," shu-cpp-token-type-op 22 22))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-6" token-info "thingbob" shu-cpp-token-type-uq 14 21))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-7" token-info "&" shu-cpp-token-type-op 13 13))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-8" token-info "ThingBob" shu-cpp-token-type-uq 3 10))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "remove-templates-9" token-info "(" shu-cpp-token-type-op 2 2))
      (setq tlist (cdr tlist))
      )
    ))



;;
;;  shu-test-shu-cpp-adjust-template-parameters
;;
(ert-deftest shu-test-shu-cpp-adjust-template-parameters ()
  "Test SHU-CPP-ADJUST-TEMPLATE-PARAMETERS"
  (let ((token-list)
        (tlist)
        (token-info))
    (with-temp-buffer
      (insert
       (concat
        "\n"
        "(ThingBob  &thingbob,\n"
        " Mumble<int, double>   &mumb1)"))
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "(" ")"))
      (should (shu-cpp-is-reverse-token-list-balanced token-list "<" ">"))
      (shu-cpp-adjust-template-parameters token-list)
      (setq tlist token-list)
      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-1" token-info ")" shu-cpp-token-type-op 53 53))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-2" token-info "mumb1" shu-cpp-token-type-uq 48 52))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-3" token-info "&" shu-cpp-token-type-op 47 47))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-4" token-info "int, double" shu-cpp-token-type-tp 32 42))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-4" token-info "Mumble" shu-cpp-token-type-uq 25 30))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-5" token-info "," shu-cpp-token-type-op 22 22))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-6" token-info "thingbob" shu-cpp-token-type-uq 14 21))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-7" token-info "&" shu-cpp-token-type-op 13 13))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-8" token-info "ThingBob" shu-cpp-token-type-uq 3 10))
      (setq tlist (cdr tlist))

      (setq token-info (car tlist))
      (should (shu-test-check-shu-cpp-get-token "adjust-templates-9" token-info "(" shu-cpp-token-type-op 2 2))
      (setq tlist (cdr tlist))
      )
    ))



;;
;;  shu-test-shu-cpp-token-extract-type-1
;;
(ert-deftest shu-test-shu-cpp-token-extract-type-1 ()
  (let ((token "// Hi there")
        (ttype shu-cpp-token-type-ct)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (token-type))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= shu-cpp-token-type-ct token-type))
    ))



;;
;;  shu-test-shu-cpp-token-extract-type-2
;;
(ert-deftest shu-test-shu-cpp-token-extract-type-2 ()
  (let ((token "\"Hi there, how are you?\"")
        (ttype shu-cpp-token-type-qt)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (token-type))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= shu-cpp-token-type-qt token-type))
    ))


;;
;;  shu-test-shu-cpp-token-extract-token-1
;;
(ert-deftest shu-test-shu-cpp-token-extract-token-1 ()
  (let ((token "// Hi there")
        (ttype shu-cpp-token-type-ct)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (xtoken))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq xtoken (shu-cpp-token-extract-token token-info))
    (should xtoken)
    (should (stringp xtoken))
    (should (string= token xtoken))
    ))


;;
;;  shu-test-shu-cpp-token-extract-spoint-1
;;
(ert-deftest shu-test-shu-cpp-token-extract-spoint-1 ()
  (let ((token "// Hi there")
        (ttype shu-cpp-token-type-ct)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (xspoint))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq xspoint (shu-cpp-token-extract-spoint token-info))
    (should xspoint)
    (should (numberp xspoint))
    (should (= spoint xspoint))
    ))


;;
;;  shu-test-shu-cpp-token-extract-epoint-1
;;
(ert-deftest shu-test-shu-cpp-token-extract-epoint-1 ()
  (let ((token "// Hi there")
        (ttype shu-cpp-token-type-ct)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (xepoint))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq xepoint (shu-cpp-token-extract-epoint token-info))
    (should xepoint)
    (should (numberp xepoint))
    (should (= epoint xepoint))
    ))



;;
;;  shu-test-shu-cpp-token-is-comment-1
;;
(ert-deftest shu-test-shu-cpp-token-is-comment-1 ()
  (let ((token "// Hi there")
        (ttype shu-cpp-token-type-ct)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (is-comment))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq is-comment (shu-cpp-token-is-comment token-info))
    (should is-comment)
    ))



;;
;;  shu-test-shu-cpp-token-is-comment-2
;;
(ert-deftest shu-test-shu-cpp-token-is-comment-2 ()
  (let ((token "// Hi there")
        (ttype shu-cpp-token-type-cc)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (is-comment))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq is-comment (shu-cpp-token-is-comment token-info))
    (should is-comment)
    ))



;;
;;  shu-test-shu-cpp-token-is-comment-3
;;
(ert-deftest shu-test-shu-cpp-token-is-comment-3 ()
  (let ((token "MumbleFrotz")
        (ttype shu-cpp-token-type-uq)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (is-comment))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq is-comment (shu-cpp-token-is-comment token-info))
    (should (not is-comment))
    ))



;;
;;  shu-test-shu-cpp-token-next-non-comment-1
;;
(ert-deftest shu-test-shu-cpp-token-next-non-comment-1 ()
  (let ((token "// Hi there")
        (ttype shu-cpp-token-type-cc)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (tlist)
        (nlist))
    (setq token-info (shu-cpp-make-token-info token ttype spoint epoint))
    (setq tlist (cons token-info tlist))
    (should (not nlist))
    ))



;;
;;  shu-test-shu-cpp-token-next-non-comment-2
;;
(ert-deftest shu-test-shu-cpp-token-next-non-comment-2 ()
  (let ((token1 "// Hi there")
        (token2 "MumbleFrotz")
        (ttype1 shu-cpp-token-type-cc)
        (ttype2 shu-cpp-token-type-uq)
        (ttype3)
        (spoint 8092)
        (epoint 9034)
        (token-info)
        (tlist)
        (nlist))
    (setq token-info (shu-cpp-make-token-info token2 ttype2 spoint epoint))
    (setq tlist (cons token-info tlist))
    (setq token-info (shu-cpp-make-token-info token1 ttype1 spoint epoint))
    (setq tlist (cons token-info tlist))
    (setq nlist (shu-cpp-token-next-non-comment tlist))
    (should nlist)
    (setq token-info (car nlist))
    (should token-info)
    (setq ttype3 (shu-cpp-token-extract-type token-info))
    (should ttype3)
    (should (numberp ttype3))
    (should (= ttype3 shu-cpp-token-type-uq))
    ))



;;
;;  shu-test-shu-cpp-token-next-non-comment-3
;;
(ert-deftest shu-test-shu-cpp-token-next-non-comment-3 ()
  (let ((token-list)
        (token-info)
        (tlist)
        (count 0)
        (ncount 0)
        (comment-count 0)
        (limit)
        (this)
        (data
         (concat
          "    std:string   /* Hi! */  x;\n"
          "    x =\"This is a fine kettle of fish is it not?\" // Again\n"
          "    int  j; /* again */\n"
          "    j++;\n")))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max))))
    (setq tlist token-list)
    (while tlist
      (setq count (1+ count))
      (setq token-info (car tlist))
      (when (shu-cpp-token-is-comment token-info)
        (setq comment-count (1+ comment-count)))
      (setq this (shu-cpp-token-string-token-info token-info))
      (setq tlist (cdr tlist)))
    (should (> comment-count 0))
    (setq limit (- count comment-count))
    (setq tlist token-list)
    (while tlist
      (setq ncount (1+ ncount))
      (should (not (> ncount limit)))
      (setq token-info (car tlist))
      (should (not (shu-cpp-token-is-comment token-info)))
      (setq tlist (shu-cpp-token-next-non-comment tlist)))
    (should (= limit ncount))
    ))




;;
;;  shu-test-shu-cpp-token-first-non-comment-1
;;
(ert-deftest shu-test-shu-cpp-token-first-non-comment-1 ()
  (let (
        (token-list)
        (token-info)
        (token-type)
        (tlist)
        (count 0)
        (ncount 0)
        (comment-count 0)
        (limit)
        (this)
        (first-non-comment-token-info)
        (data
         (concat
          "    // This is a comment\n"
          "  /* and yet another comment */\n"
          "    x =\"This is a fine kettle of fish is it not?\" // Again\n"
          "    int  j; /* again */\n"
          "    j++;\n")))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
    (setq tlist token-list)
    (while (and tlist (not first-non-comment-token-info))
      (setq count (1+ count))
      (setq token-info (car tlist))
      (when (not (shu-cpp-token-is-comment token-info))
        (setq first-non-comment-token-info token-info)
        )
      (setq tlist (cdr tlist))
      )
    (setq tlist token-list)
    (setq tlist (shu-cpp-token-first-non-comment tlist))
    (should tlist)
    (should (consp tlist))
    (setq token-info (car tlist))
    (should (equal first-non-comment-token-info token-info))
    ))


;;
;;  shu-=test-shu-get-cpp-keywords-hash
;;
(ert-deftest shu-=test-shu-get-cpp-keywords-hash ()
  (let ((kl shu-cpp-keywords)
        (ht (shu-get-cpp-keywords-hash))
        (kc)
        (kw)
        (nkw))
    (while kl
      (setq kc (car kl))
      (setq kw (car kc))
      (should (gethash kw ht))
      (setq nkw (concat kw "something-or-other"))
      (should (not (gethash nkw ht)))
      (setq kl (cdr kl)))
    ))



;;
;;  shu-test-shu-cpp-token-set-nesting-level
;;
(ert-deftest shu-test-shu-cpp-token-set-nesting-level ()
  (let ((token-info)
        (token "TOKEN")
        (token-type shu-cpp-token-type-qt)
        (spoint 123)
        (epoint 456)
        (error-message "Hi there")
        (nesting-level 12)
        (xnesting-level))
    (setq token-info (shu-cpp-make-token-info token token-type spoint epoint error-message))
    (shu-cpp-token-set-nesting-level token-info nesting-level)
    (setq xnesting-level (shu-cpp-token-extract-nesting-level token-info))
    (should xnesting-level)
    (should (numberp xnesting-level))
    (should (= nesting-level nesting-level))
    ))


;;; shu-cpp-token.t.el ends here
