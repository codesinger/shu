;;; shu-cpp-match.t.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-match
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

;; Unit tests for the functions in shu-cpp-match.el

;;; Code:

(require 'ert)
(require 'shu-cpp-match)



;;
;;  shu-test-shu-cpp-make-match-info-1
;;
(ert-deftest shu-test-shu-cpp-make-match-info-1 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (match-info)
        (op-code shu-cpp-token-match-type-same)
        (match-eval-func 'shu-cpp-token-match-same)
        (match-ret-ind nil)
        (match-token-type shu-cpp-token-type-kw)
        (match-token-value "using")
        (xop-code)
        (xmatch-eval-func)
        (xmatch-ret-ind)
        (xmatch-token-type)
        (xmatch-token-value))
    (setq match-info (shu-cpp-make-match-info op-code match-eval-func
                                              match-ret-ind match-token-type
                                              match-token-value))
    (shu-cpp-match-extract-info match-info xop-code xmatch-eval-func
                                xmatch-ret-ind xmatch-token-type
                                xmatch-token-value)
    (should xop-code)
    (should (numberp xop-code))
    (should (= op-code xop-code))
    (should match-eval-func)
    (should (functionp match-eval-func))
    (should (equal match-eval-func xmatch-eval-func))
    (should (not xmatch-ret-ind))
    (should xmatch-token-type)
    (should (numberp xmatch-token-type))
    (should (= xmatch-token-type match-token-type))
    (should xmatch-token-value)
    (should (stringp xmatch-token-value))
    (should (string= match-token-value xmatch-token-value))
    ))


;;
;;  shu-test-shu-cpp-match-extract-token-1
;;
(ert-deftest shu-test-shu-cpp-match-extract-token-1 ()
  (let ((data
         (cons shu-cpp-token-match-type-same
               (cons
                (cons nil 'shu-cpp-token-match-same)
                (cons shu-cpp-token-type-kw "using"))))
        (expected "using")
        (actual))
    (setq actual (shu-cpp-match-extract-token data))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;
;;  shu-test-shu-cpp-match-extract-type-1
;;
(ert-deftest shu-test-shu-cpp-match-extract-type-1 ()
  (let ((data
         (cons shu-cpp-token-match-type-same
               (cons
                (cons nil 'shu-cpp-token-match-same)
                (cons shu-cpp-token-type-kw "using"))))
        (expected shu-cpp-token-type-kw)
        (actual))
    (setq actual (shu-cpp-match-extract-type data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))


;;
;;  shu-test-shu-cpp-match-extract-op-code-1
;;
(ert-deftest shu-test-shu-cpp-match-extract-op-code-1 ()
  (let ((data
         (cons shu-cpp-token-match-type-same
               (cons
                (cons nil 'shu-cpp-token-match-same)
                (cons shu-cpp-token-type-kw "using"))))
        (expected shu-cpp-token-match-type-same)
        (actual))
    (setq actual (shu-cpp-match-extract-op-code data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-match-side-list-1
;;
(ert-deftest shu-test-shu-cpp-make-match-side-list-1 ()
  (let (
        (op-code 101)
        (match-list 303)
        (side-parameter)
        (match-info)
        (xop-code)
        (xmatch-list)
        (xside-parameter)
        )
    (setq match-info (shu-cpp-make-match-side-list op-code match-list))
    (shu-cpp-match-extract-side-list match-info xop-code xmatch-list xside-parameter)
    (should (= op-code xop-code))
    (should (= match-list xmatch-list))
    (should (equal side-parameter xside-parameter))

    (setq op-code 202)
    (setq match-list 303)
    (setq side-parameter 404)
    (setq match-info (shu-cpp-make-match-side-list op-code match-list side-parameter))
    (shu-cpp-match-extract-side-list match-info xop-code xmatch-list xside-parameter)
    (should (= op-code xop-code))
    (should (= match-list xmatch-list))
    (should (= side-parameter xside-parameter))
    ))



;;
;;  shu-test-shu-cpp-match-is-side-list-1
;;
(ert-deftest shu-test-shu-cpp-match-is-side-list-1 ()
  (let ((op-code shu-cpp-token-match-type-same)
        (actual))
    (setq actual (shu-cpp-match-is-side-list op-code))
    (should (not actual))
    ))



;;
;;  shu-test-shu-cpp-match-is-side-list-2
;;
(ert-deftest shu-test-shu-cpp-match-is-side-list-2 ()
  (let ((op-code shu-cpp-token-match-type-side-loop)
        (actual))
    (setq actual (shu-cpp-match-is-side-list op-code))
    (should actual)
    ))




;;
;;  shu-test-shu-cpp-token-match-skip
;;
(ert-deftest shu-test-shu-cpp-token-match-skip ()
  (let (
        (tlist
         (list
          "a"
          "b"))
        (nlist)
        (expected "b")
        (actual)
        )
    (setq nlist (shu-cpp-token-match-skip tlist))
    (should nlist)
    (should (consp nlist))
    (setq actual (car nlist))
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-token-match-same-1
;;
(ert-deftest shu-test-shu-cpp-token-match-same-1 ()
  (let* (
         (token "using")
         (token-type shu-cpp-token-type-kw)
         (spoint 1022)
         (epoint 1026)
         (token-info (shu-cpp-make-token-info token token-type spoint epoint))
         (data
          (cons shu-cpp-token-match-type-same
                (cons
                 (cons nil 'shu-cpp-token-match-same)
                 (cons token-type token))))
         (is-same)
         )
    (should (shu-cpp-token-match-same data token-info))
    ))


;;
;; The data below has to become a list of lists
;; The list below can find
;;   - "using"
;;   - "namespace"
;;   - namespace name
;;
;; We need another that finds
;;   - "using"
;;   - "namespace"
;;   - "::"
;;   - namespace name
;;
;; That gives us a list of length two, each of
;; which is a list

;;
;;  shu-test-shu-cpp-match-tokens-1
;;
(ert-deftest shu-test-shu-cpp-match-tokens-1 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (ret-val)
        (rlist)
        (token-info)
        (match-lists
         (list
          (list
           (cons shu-cpp-token-match-type-skip
                 (cons
                  (cons nil 'shu-cpp-token-match-skip)
                  (cons 0 nil)))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "using")))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "namespace")))
           (cons shu-cpp-token-match-type-same-rx
                 (cons
                  (cons t 'shu-cpp-token-match-same-rx)
                  (cons shu-cpp-token-type-uq (concat shu-cpp-name "+")))))))
        (data
         (concat
          "x using namespace fumble;\n"
          ))
        (token-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (princ token-list gb) (princ "\n\n" gb)
    (let ((tlist token-list)
          (tinfo)
          (count 0)
          (name))
      (while tlist
        (setq count (1+ count))
        (setq tinfo (car tlist))
        (setq name (shu-cpp-token-string-token-info tinfo))
        (princ (format "%d, %s\n" count name) gb)
        (setq tlist (cdr tlist)))
      )
    (let ((mlist)
          (match-info)
          (op-code)
          (match-eval-func)
          (match-ret-ind)
          (match-token-type)
          (match-token-value)
          (count 0))
      (setq mlist (car match-lists))
      (while mlist
        (setq count (1+ count))
        (setq match-info (car mlist))
        (shu-cpp-match-extract-info match-info op-code match-eval-func
                                    match-ret-ind match-token-type match-token-value)
        (princ (format "%d, %s\n" count match-token-value) gb)
        (setq mlist (cdr mlist))))
    (setq ret-val (shu-cpp-match-tokens rlist match-lists token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq token-info (cdr ret-val))
    (should token-info)
    ))



;;
;; The data below has to become a list of lists
;; The list below can find
;;   - "using"
;;   - "namespace"
;;   - namespace name
;;
;; We need another that finds
;;   - "using"
;;   - "namespace"
;;   - "::"
;;   - namespace name
;;
;; That gives us a list of length two, each of
;; which is a list

;;
;;  shu-test-shu-cpp-match-tokens-2
;;
(ert-deftest shu-test-shu-cpp-match-tokens-2 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (ret-val)
        (token-info)
        (match-lists
         (list
          (list
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "using")))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "namespace")))
           (cons shu-cpp-token-match-type-same-rx
                 (cons
                  (cons t 'shu-cpp-token-match-same-rx)
                  (cons shu-cpp-token-type-uq (concat shu-cpp-name "+")))))))
        (data
         (concat
          "x using namespace fumble;\n"
          "class blah {};\n"
          ))
        (token-list)
        (olist))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max))))
    (princ token-list gb) (princ "\n\n" gb)
    (let ((tlist token-list)
          (tinfo)
          (count 0)
          (name))
      (while tlist
        (setq count (1+ count))
        (setq tinfo (car tlist))
        (push tinfo olist)
        (setq name (shu-cpp-token-string-token-info tinfo))
        (princ (format "%d, %s\n" count name) gb)
        (setq tlist (cdr tlist))))
    (let ((mlist)
          (match-info)
          (op-code)
          (match-eval-func)
          (match-ret-ind)
          (match-token-type)
          (match-token-value)
          (count 0))
      (setq mlist (car match-lists))
      (while mlist
        (setq count (1+ count))
        (setq match-info (car mlist))
        (shu-cpp-match-extract-info match-info op-code match-eval-func
                                    match-ret-ind match-token-type match-token-value)
        (princ (format "%d, %s\n" count match-token-value) gb)
        (setq mlist (cdr mlist))))
    (let ((tlist olist)
          (rlist)
          (tinfo)
          (count 0)
          (name)
          (token)
          (token-type))
      (princ "\n\n" gb)
      (while tlist
        (setq count (1+ count))
        (setq tinfo (car tlist))
        (push tinfo olist)
        (setq name (shu-cpp-token-string-token-info tinfo))
        (princ (format "%d, %s\n" count name) gb)
        (setq token-type (shu-cpp-token-extract-type tinfo))
        (setq token (shu-cpp-token-extract-token tinfo))
        (when (and (= token-type shu-cpp-token-type-kw)
                   (string= token "using"))
          (setq ret-val (shu-cpp-match-tokens rlist match-lists tlist))
          (should ret-val)
          (should (consp ret-val))
          (setq rlist (cdr ret-val))
          (should rlist))
        (setq tlist (cdr tlist))))
    ))



;;
;;  shu-test-shu-cpp-match-tokens-3
;;
(ert-deftest shu-test-shu-cpp-match-tokens-3 ()
  (let ((token)
        (match-lists
         (list
          (list  ;; "using namespace <name>;"
           (cons shu-cpp-token-match-type-skip
                 (cons
                  (cons nil 'shu-cpp-token-match-skip)
                  (cons 0 nil)))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "using")))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "namespace")))
           (cons shu-cpp-token-match-type-same-rx
                 (cons
                  (cons t 'shu-cpp-token-match-same-rx)
                  (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons t 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-op ";"))))
          (list  ;;  "using namespace ::std;"
           (cons shu-cpp-token-match-type-skip
                 (cons
                  (cons nil 'shu-cpp-token-match-skip)
                  (cons 0 nil)))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "using")))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-kw "namespace")))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-op "::")))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons t 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-uq "std")))
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons t 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-op ";"))))))
        (data
         (concat
          "  I using namespace ::std;\n"
          "  // hello\n"))
        (ret-val)
        (token-list)
        (rlist)
        (nrl)
        (tlist)
        (token-info)
        (token-type)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (setq count (1+ count))
      (setq tlist (cdr tlist)))

    (setq ret-val (shu-cpp-match-tokens rlist match-lists token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (should (= 2 (length rlist)))

    (setq nrl rlist)
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "std"))
    ))



;;
;;  shu-test-cpp-brace-colon-or-list
;;
(defconst shu-test-cpp-brace-colon-or-list
  (list  ;; operator "{" or operator ":"
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             nil shu-cpp-token-type-op
                             "{")
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             nil shu-cpp-token-type-op
                             ":")
   )
  )



;;
;;  shu-test-shu-cpp-match-or-list-1
;;
(ert-deftest shu-test-shu-cpp-match-or-list-1 ()
  (let ((match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-choose
                                                  shu-test-cpp-brace-colon-or-list))
        (data "{ something }")
        (token-list)
        (rlist)
        (ret-val)
        (new-token-list)
        (token-info)
        (new-rlist)
        (token-type)
        (token))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-or-list rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should (not new-rlist))
    (should new-token-list)
    (should (listp new-token-list))
    (setq token-info (car new-token-list))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "something"))
    ))



;;
;;  shu-test-shu-cpp-match-or-list-2
;;
(ert-deftest shu-test-shu-cpp-match-or-list-2 ()
  (let ((match-info
         (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-choose
                                       (list  ;; operator "{" or operator ":"
                                        (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                                                  'shu-cpp-token-match-same
                                                                  nil shu-cpp-token-type-op
                                                                  "{")
                                        (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                                                  'shu-cpp-token-match-same
                                                                  t shu-cpp-token-type-op
                                                                  ":"))))
        (data ": something :")
        (token-list)
        (rlist)
        (ret-val)
        (new-token-list)
        (token-info)
        (new-rlist)
        (token-type)
        (token))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-or-list rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should new-rlist)
    (should (listp new-rlist))
    (setq token-info (car new-rlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should token)
    (should (stringp token))
    (should (string= token ":"))
    (should new-token-list)
    (should (listp new-token-list))
    (setq token-info (car new-token-list))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "something"))
    ))



;;
;;  shu-test-shu-cpp-match-or-list-3
;;
(ert-deftest shu-test-shu-cpp-match-or-list-3 ()
  (let ((match-info
         (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-choose
                                       (list  ;; operator "{" or operator ":"
                                        (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                                                  'shu-cpp-token-match-same
                                                                  nil shu-cpp-token-type-op
                                                                  "{")
                                        (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                                                  'shu-cpp-token-match-same
                                                                  t shu-cpp-token-type-op
                                                                  ":"))))
        (data ": something :")
        (rlist-data " namespace using ")
        (token-list)
        (rlist)
        (ret-val)
        (new-token-list)
        (token-info)
        (new-rlist)
        (nrl)
        (token-type)
        (token))
    (with-temp-buffer
      (insert rlist-data)
      (setq rlist (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-or-list rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should new-rlist)
    (should (listp new-rlist))
    (setq nrl new-rlist)

    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should token)
    (should (stringp token))
    (should (string= token ":"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nrl (cdr nrl))
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (should new-token-list)
    (should (listp new-token-list))
    (setq token-info (car new-token-list))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "something"))
    ))



;;
;;  shu-test-shu-cpp-match-or-list-4
;;
(ert-deftest shu-test-shu-cpp-match-or-list-4 ()
  (let ((match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-choose
                                                  shu-test-cpp-brace-colon-or-list))
        (data "; something ;")
        (token-list)
        (rlist)
        (ret-val))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-or-list rlist token-list match-info))
    (should (not ret-val))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-sub-list-1
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-1 ()
  "Whole list matches, updated rlist and tlist returned."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")
          )
         )
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list nil))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (shu-cpp-token-show-token-info new-rlist "NEW_RLIST 8889")
    (shu-cpp-token-show-token-info new-token-list "NEW_TOKEN-LIST 8889")
    (princ "new-rlist: " gb)(princ new-rlist gb)(princ "\n" gb)
    (princ "new-token-list: " gb)(princ new-token-list gb)(princ "\n" gb)

    (setq token-info (car new-rlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "bbbb"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token "::"))
    ))





;;
;;  shu-test-shu-cpp-match-repeat-sub-list-2
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-2 ()
  "First item does not match.  Original and rlist returned."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " ; bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list nil))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (shu-cpp-token-show-token-info new-rlist "NEW_RLIST 8889")
    (shu-cpp-token-show-token-info new-token-list "NEW_TOKEN-LIST 8889")
    (princ "new-rlist-2: " gb)(princ new-rlist gb)(princ "\n" gb)
    (princ "new-token-list-2: " gb)(princ new-token-list gb)(princ "\n" gb)
    (should (eq rlist new-rlist))
    (should (eq token-list new-token-list))
    ))





;;
;;  shu-test-shu-cpp-match-repeat-sub-list-3
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-3 ()
  "Second item does not match, nil returned."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list nil))
    (should (not ret-val))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-sub-list-4
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-4 ()
  "Third item does not match, nil returned."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list nil))
    (should (not ret-val))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-sub-list-5
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-5 ()
  "Token list is too short, nil returned."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  "
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list nil))
    (should (not ret-val))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-sub-list-6
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-6 ()
  "Token list is empty.  Original token-list and rlist returned."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " ; bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq rlist token-list)
    (setq token-list nil)
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list nil))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (shu-cpp-token-show-token-info new-rlist "NEW_RLIST 8889")
    (shu-cpp-token-show-token-info new-token-list "NEW_TOKEN-LIST 8889")
    (princ "new-rlist-2: " gb)(princ new-rlist gb)(princ "\n" gb)
    (princ "new-token-list-2: " gb)(princ new-token-list gb)(princ "\n" gb)
    (should (eq rlist new-rlist))
    (should (eq token-list new-token-list))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-sub-list-7
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-7 ()
  "Whole list matches (with first mtvh required) , updated rlist and tlist
returned."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")
          )
         )
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list t))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (shu-cpp-token-show-token-info new-rlist "NEW_RLIST 8889")
    (shu-cpp-token-show-token-info new-token-list "NEW_TOKEN-LIST 8889")
    (princ "new-rlist: " gb)(princ new-rlist gb)(princ "\n" gb)
    (princ "new-token-list: " gb)(princ new-token-list gb)(princ "\n" gb)

    (setq token-info (car new-rlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "bbbb"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token "::"))
    ))





;;
;;  shu-test-shu-cpp-match-repeat-sub-list-8
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-8 ()
  "First item is required but does not match.  nil returned."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " ; bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list t))
    (should (not ret-val))
    ))



;;
;;  shu-test-shu-cpp-match-repeat-list-1
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-1 ()
  (let ((data
         (concat
          " :: bbbb"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-list rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should (not new-token-list))
    (should new-rlist)
    (shu-cpp-tokenize-show-list new-rlist)
    (should (listp new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "bbbb"))
    ))



;;
;;  shu-test-shu-cpp-match-repeat-list-2
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-2 ()
  (let ((data
         (concat
          " :: bbbb"
          "    cccc"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-list rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should new-token-list)
    (should (listp new-token-list))
    (should new-rlist)
    (shu-cpp-tokenize-show-list new-rlist)
    (should (listp new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "bbbb"))
    ))



;;
;;  shu-test-shu-cpp-match-repeat-list-3
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-3()
  (let ((data
         (concat
          "  bbbb"
          "  cccc"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-list rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should (not new-rlist))
    (should new-token-list)
    (setq token-info (car new-token-list))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "bbbb"))
    ))



;;
;;  shu-test-shu-cpp-match-repeat-list-4
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-4 ()
  (let ((data
         (concat
          "    ::  zzzz"
          " :: yyyy"
          " ::   qqqq"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-list rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should (not new-token-list))
    (should new-rlist)
    (should (listp new-rlist))
    (shu-cpp-tokenize-show-list new-rlist)
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "qqqq"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "yyyy"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "zzzz"))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-list-5
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-5 ()
  "Mis-match in itemm two of fourth iteration.  Should return nil."
  (let ((data
         (concat
          "    ::  zzzz"
          " :: yyyy"
          " :: wwww"
          " ::  ;  "
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (rlist-data " namespace using ")
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (title)
        (nrl))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert rlist-data)
      (setq rlist (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (shu-cpp-tokenize-show-list rlist "RLIST IN TEST5:"))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq title (format "\nRLIST IN TEST5 BEFORE CALL, length: %d: " (length rlist)))
    (shu-cpp-tokenize-show-list rlist title)
    (setq ret-val (shu-cpp-match-repeat-list rlist token-list match-info))
    (should (not ret-val))
    ))



;;
;;  shu-test-shu-cpp-match-repeat-list-6
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-6 ()
  "Mis-match in item two of third iteration.  Should return nil."
  (let ((data
         (concat
          "    ::  zzzz"
          " :: yyyy"
          " :: :: wwww"
          " ::  ;  "
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (rlist-data " namespace using ")
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (title)
        (nrl))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert rlist-data)
      (setq rlist (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (shu-cpp-tokenize-show-list rlist "RLIST IN TEST6:"))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq title (format "\nRLIST IN TEST6 BEFORE CALL, length: %d: " (length rlist)))
    (shu-cpp-tokenize-show-list rlist title)
    (setq ret-val (shu-cpp-match-repeat-list rlist token-list match-info))
    (should (not ret-val))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-list-7
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-7 ()
  "Mis-match in item 2 of first iteration.  Should return nil."
  (let ((data
         (concat
          " :: :: bbbb"
          "    cccc"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-list rlist token-list match-info))
    (should (not ret-val))
    ))


;;
;;  shu-test-shu-cpp-match-repeat-list-once-1
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-once-1 ()
  "Mis-match in first item.  Should return nil."
  (let ((data
         (concat
          "      zzzz"
          " :: yyyy"
          " :: :: wwww"
          " ::  ;  "
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (rlist-data " namespace using ")
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (title)
        (nrl))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                   match-list))
    (with-temp-buffer
      (insert rlist-data)
      (setq rlist (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (shu-cpp-tokenize-show-list rlist "RLIST IN TEST6:"))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq title (format "\nRLIST IN TEST6 BEFORE CALL, length: %d: " (length rlist)))
    (shu-cpp-tokenize-show-list rlist title)
    (setq ret-val (shu-cpp-match-repeat-list-once rlist token-list match-info))
    (should (not ret-val))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-list-once-2
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-once-2 ()
  (let ((data
         (concat
          "    ::  zzzz"
          " :: yyyy"
          " ::   qqqq"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop-once
                                                   match-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-list-once rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should (not new-token-list))
    (should new-rlist)
    (should (listp new-rlist))
    (shu-cpp-tokenize-show-list new-rlist)
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "qqqq"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "yyyy"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "zzzz"))
    ))




;;
;;  shu-test-shu-cpp-match-repeat-list-once-3
;;
(ert-deftest shu-test-shu-cpp-match-repeat-list-once-3 ()
  (let ((data
         (concat
          "    ::  zzzz\n"
          " :: yyyy\n"
          " ::   qqqq    it;\n"
          "// A command\n"
          "  std::string   other;\n"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (setq match-info (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop-once
                                                   match-list))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-list-once rlist token-list match-info))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (should new-token-list)
    (should (listp new-token-list))
    (should new-rlist)
    (should (listp new-rlist))
    (shu-cpp-tokenize-show-list new-rlist)
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "qqqq"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "yyyy"))

    (setq new-rlist (cdr new-rlist))
    (setq token-info (car new-rlist))
    (should token-info)
    (should (consp token-info))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token ( shu-cpp-token-extract-token token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should token)
    (should (stringp token))
    (should (string= token "zzzz"))
    ))



;;
;;  shu-test-shu-cpp-colon-name
;;
(defconst shu-test-shu-cpp-colon-name
  (list
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             nil shu-cpp-token-type-op
                             "::")
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                             'shu-cpp-token-match-same-rx
                             t shu-cpp-token-type-uq
                             (concat shu-cpp-name "+")))
  "A repeating side list to match :: <name>")



;;
;;  shu-cpp-namespace-match-list-2
;;
(defconst shu-cpp-namespace-match-list-2
  (list
   (list  ;; "using namespace <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";"))


   (list  ;; "using namespace :: <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                              "::")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";"))


   (list  ;; "using namespace <name> {:: <name>};"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))

    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                  shu-test-shu-cpp-colon-name)
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";"))))



;;
;;  test-shu-cpp-match-tokens-with-ns1
;;
(ert-deftest test-shu-cpp-match-tokens-with-ns1 ()
  (let ((ret-val)
        (rlist)
        (token-list)
        (new-token-list)
        (data
         (concat
          "  using namespace fred ; \n"
          "  using namespace bob ; \n"
          "  using namespace andy ; \n"))
        (debug-on-error t))

    (shu-cpp-token-show-match-lists shu-cpp-namespace-match-list-2 "\nshu-cpp-namespace-match-list-2\n")
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-2 token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should new-token-list)
    (should (listp new-token-list))
    (shu-cpp-tokenize-show-list rlist)

    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-2 new-token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should new-token-list)
    (should (listp new-token-list))
    (shu-cpp-tokenize-show-list rlist)

    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-2 new-token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should (not new-token-list))
    (shu-cpp-tokenize-show-list rlist)
    ))




;;
;;  test-shu-cpp-match-tokens-with-ns2
;;
(ert-deftest test-shu-cpp-match-tokens-with-ns2 ()
  (let ((ret-val)
        (rlist)
        (nrl)
        (token)
        (token-info)
        (token-type)
        (token-list)
        (new-token-list)
        (data
         (concat
          "  using namespace fred ; \n"
          "  using namespace bob ; \n"
          "  using namespace /* Hello */ alice::ted::jim ; \n"))
        (debug-on-error t))

    (shu-cpp-token-show-match-lists shu-cpp-namespace-match-list-2 "\nshu-cpp-namespace-match-list-2\n")
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-2 token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should new-token-list)
    (should (listp new-token-list))
    (shu-cpp-tokenize-show-list rlist "NS2")

    (setq nrl rlist)
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "fred"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-2 new-token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should new-token-list)
    (should (listp new-token-list))
    (shu-cpp-tokenize-show-list rlist "NS2")

    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-2 new-token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should (not new-token-list))
    (shu-cpp-tokenize-show-list new-token-list "NS2")
    (shu-cpp-tokenize-show-list rlist "NS2")

    (setq nrl rlist)
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "jim"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "ted"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "alice"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ))




;;
;;  test-shu-cpp-match-tokens-with-ns3
;;
(ert-deftest test-shu-cpp-match-tokens-with-ns3 ()
  (let ((ret-val)
        (rlist)
        (nrl)
        (token)
        (token-type)
        (token-list)
        (new-token-list)
        (data
         (concat
          "  using namespace alice :: ; ted::jim ; \n")))
    (shu-cpp-token-show-match-lists shu-cpp-namespace-match-list-2 "\nshu-cpp-namespace-match-list-2\n")
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-2 token-list))
    (should (not ret-val))
    ))


;;; This works as of 28 July 2019


(require 'ert)
(require 'shu-cpp-match)




(defconst shu-test-shu-cpp-colon-name
  (list
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             nil shu-cpp-token-type-op
                             "::")
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                             'shu-cpp-token-match-same-rx
                             t shu-cpp-token-type-uq
                             (concat shu-cpp-name "+"))
   )
  "A repeating side list to match :: <name>")


;;
;;  shu-cpp-match-namespace-forms
;;
;;
;;                                  using
;;                                    |
;;                                    V
;;                                namespace
;;                                    |
;;                                    |
;;          +-------------------------+-------------------------+
;;          |                         |                         |
;;          |                         |                         |
;;          V                         V                         V
;;        <name>                     ::                       <name>
;;          |                         |                         |
;;          |                         |                         |
;;          |                         V                         V
;;          |                       <name>           loop of :: followed by <name>
;;          |                         |                         |
;;          |                         |                         |
;;          V                         V                         V
;;          ;                         ;                         ;
;;
;;
;;
(defconst shu-cpp-match-namespace-forms
  (list
   (list  ;; "<name>"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";"))


   (list  ;; ":: <name>"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                              "::")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";"))


   (list  ;; "<name> {:: <name>};"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))

    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                  shu-test-shu-cpp-colon-name)
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";"))))


;;
;;  shu-cpp-namespace-match-list-3
;;
(defconst shu-cpp-namespace-match-list-3
  (list
   (list  ;; "using namespace <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                  shu-cpp-match-namespace-forms))))





;;
;;  test-shu-cpp-match-tokens-with-ns4
;;
(ert-deftest test-shu-cpp-match-tokens-with-ns4 ()
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (ret-val)
        (rlist)
        (nrl)
        (token-info)
        (token)
        (token-type)
        (token-list)
        (new-token-list)
        (data
         (concat
          " using  namespace fred; \n "
          "  using namespace :: bob ; \n"
          "  using namespace alice :: jeff :: ted ; \n"))
        (debug-on-error t))

    (shu-cpp-token-show-match-lists shu-cpp-namespace-match-list-3 "\nshu-cpp-namespace-match-list-3\n")
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-3 token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (shu-cpp-tokenize-show-list rlist "Returned rlist")
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should new-token-list)
    (should (listp new-token-list))
    (shu-cpp-tokenize-show-list rlist "NS2")

    (setq nrl rlist)
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "fred"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))


    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-3 new-token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (shu-cpp-tokenize-show-list rlist "Returned rlist")
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should new-token-list)
    (shu-cpp-tokenize-show-list rlist "NS2")

    (setq nrl rlist)
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "bob"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))


    (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list-3 new-token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (shu-cpp-tokenize-show-list rlist "Returned rlist")
    (should rlist)
    (should (listp rlist))
    (setq new-token-list (car ret-val))
    (should (not new-token-list))
    (shu-cpp-tokenize-show-list rlist "NS2")

    (setq nrl rlist)
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "ted"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "jeff"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "alice"))

    (setq nrl (cdr nrl))
    (should nrl)
    (setq token-info (car nrl))
    (should token-info)
    (should (consp token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should token)
    (should (stringp token))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (should token-type)
    (should (numberp token-type))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ))






;;
;;  shu-test-shu-cpp-all-search-match-tokens
;;
(ert-deftest shu-test-shu-cpp-all-search-match-tokens ()
  (let ((data
         (concat
          "  unsigned int   x;\n"
          "  double    /* Hi */     boo;\n"
          "  PriceMap       moo /* OK */;\n"
          ))
        (token-list)
        (ret-val)
        (find-name-list
         (list  ;; semicolon followed by name
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    ";")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (tlist)
        (rlist)
        (token)
        (token-type)
        (token-info)
        (ret-val))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-all-search-match-tokens rlist find-name-list token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (shu-cpp-tokenize-show-list rlist "\nRLIST 999")

    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "x"))

    (setq rlist (cdr rlist))
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "boo"))

    (setq rlist (cdr rlist))
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "moo"))

    (setq rlist (cdr rlist))
    (should (not rlist))
    ))




;;
;;  shu-test-shu-cpp-match-classname-forms-1
;;
(ert-deftest shu-test-shu-cpp-match-classname-forms-1 ()
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (token-list)
        (token-info)
        (token)
        (rlist)
        (ret-val)
        (rlist)
        (data
         (concat
          "// this is some stuff\n"
          "\n"
          "bdlt::Datetime  time;\n"
          "\n"
          "std::string   it;\n"
          "\n"
          "\n"
          "my::name::Fred  thing;\n"
          "\n"
          "Datetime  it;\n"
          "\n"
          "// Some sort of comment\n"
          "\n"
          "mumble  it\n"
          "\n"
          "bdlt::Interval  y;\n"
          "/* */\n"
          )))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-all-search-match-tokens rlist shu-cpp-match-classname-forms token-list))
      (princ "\ncar ret-val: " gb)(princ (car ret-val) gb) (princ "\n" gb)
      (princ "\ncdr ret-val: " gb)(princ (cdr ret-val) gb) (princ "\n" gb)
      (should ret-val)
      (should (consp ret-val))
      (should (cdr ret-val))
      (setq rlist (cdr ret-val))
      (should (listp rlist))
      (setq rlist (nreverse rlist))
      (shu-cpp-tokenize-show-list rlist)

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "bdlt" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "::" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "Datetime" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "std" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "::" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "string" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "my" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "::" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "name" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "::" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "Fred" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "bdlt" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "::" token))
      (setq rlist (cdr rlist))

      (should rlist)
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (should (string= "Interval" token))
      (setq rlist (cdr rlist))

      (should (not rlist)))
    ))


;;; shu-cpp-match.t.el ends here
