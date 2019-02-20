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

;; Unit tests for the functions in shiu-match.el

;;; Code:

(require 'shu-cpp-match)



;;
;;  shu-test-shu-cpp-make-match-info-1
;;
(ert-deftest shu-test-shu-cpp-make-match-info-1 ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (match-info)
        (op-code shu-cpp-token-match-type-same)
        (match-eval-func 'shu-cpp-token-match-same)
        (match-ret-ind nil)
        (match-token-type shu-cpp-token-type-uq)
        (match-token-value "using")
        (xop-code)
        (xmatch-eval-func)
        (xmatch-ret-ind)
        (xmatch-token-type)
        (xmatch-token-value)
        )
    (setq match-info (shu-cpp-make-match-info match-info op-code match-eval-func
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
                (cons shu-cpp-token-type-uq "using"))))
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
                (cons shu-cpp-token-type-uq "using"))))
        (expected shu-cpp-token-type-uq)
        (actual))
    (setq actual (shu-cpp-match-extract-type data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
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
         (token-type shu-cpp-token-type-uq)
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
;;  shu-test-shu-cpp-match-tokens
;;
(ert-deftest shu-test-shu-cpp-match-tokens ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-info)
        (match-lists
         (list
          (list
           (cons shu-cpp-token-match-type-skip
                 (cons
                  (cons nil 'shu-cpp-token-match-skip)
                  (cons 0 nil)
                  )
                 )
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-uq "using")
                  )
                 )
           (cons shu-cpp-token-match-type-same
                 (cons
                  (cons nil 'shu-cpp-token-match-same)
                  (cons shu-cpp-token-type-uq "namespace")
                  )
                 )
           (cons shu-cpp-token-match-type-same-rx
                 (cons
                  (cons t 'shu-cpp-token-match-same-rx)
                  (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))
                  )
                 )
           )
          )
         )
        (data
         (concat
          "x using namespace fumble;\n"
          ))
        (token-list)
        )
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
    (princ token-list gb) (princ "\n\n" gb)
    (let (
          (tlist token-list)
          (tinfo)
          (count 0)
          (name)
          )
      (while tlist
        (setq count (1+ count))
        (setq tinfo (car tlist))
        (setq name (shu-cpp-token-string-token-info tinfo))
        (princ (format "%d, %s\n" count name) gb)
        (setq tlist (cdr tlist))
        )

      )
    (let (
          (mlist)
          (match-info)
           (op-code)
           (match-eval-func)
           (match-ret-ind)
           (match-token-type)
           (match-token-value)
           (count 0)
          )
      (setq mlist (car match-lists))
      (while mlist
        (setq count (1+ count))
        (setq match-info (car mlist))
        (shu-cpp-match-extract-info match-info op-code match-eval-func
                                    match-ret-ind match-token-type match-token-value)
        (princ (format "%d, %s\n" count match-token-value) gb)
        (setq mlist (cdr mlist))
        )
      )
    (setq token-info (shu-cpp-match-tokens match-lists token-list))
    (should token-info)

    ))


;;; shu-cpp-match.t.el ends here
