;;; shu-cpp-namespace.el --- Shu project code for dealing wth C++ in Emacs
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

;; A collection of code dealing with namespace directives that was put into
;; shu-cpp-match..el and really does not belong there.

;;; Code:

(provide 'shu-cpp-match)
(require 'shu-cpp-token)


;;
;;  shu-cpp-namespace-match-list
;;
(defconst shu-cpp-namespace-match-list
  (list
   (list  ;; "using namespace <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
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
                               ";")
    )
   (list  ;; "using namespace <name1>::<name2>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
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
                              nil shu-cpp-token-type-op
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
   (list  ;; "using namespace <name1>::<name2>::<name3;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
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
                              nil shu-cpp-token-type-op
                               "::")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                               (concat shu-cpp-name "+"))
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
                               ";")
    )
   (list  ;;  "using namespace ::std;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                               "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                               "namespace")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                               "::")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-uq
                               "std")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                               ";")
    )
   (list  ;;  "using namespace ::bsl;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                               "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-kw
                               "namespace")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                               "::")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-uq
                               "bsl")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                               ";")
    ))
  "The list of patterns to look for to match a \"using namespace\" directive.")



;;
;;  ccc
;;
(defun ccc ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (token-info)
        (ret)
        (tlist)
        (count 0)
        )
    (save-excursion
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (setq count (1+ count))
      (princ (format "ZZ %d: " count) gb) (princ token-info gb) (princ "\n" gb)
      (setq tlist (cdr tlist))
      )
      (setq ret (shu-cpp-match-find-using token-list))
      )
    ret
    ))

;;
;; Returns a list of namespace-info
;;
;;
;;  namespace-info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> point-pair
;;        |
;;        +-------------> namespace name
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
;;
;;  shu-cpp-match-find-using
;;
(defun shu-cpp-match-find-using (token-list)
  "TOKEN-LIST is a list of tokens produced by
SHU-CPP-TOKENIZE-REGION-FOR-COMMAND.  This function finds all occurrences of
\"using namespace\" directives and returns the list of namespace-info.  Each
entry in the list contains the name of the namespace as well as the start point
and end point of the entire \"using namespace\" directive."
  (let (
        (gb (get-buffer-create "**boo**"))
        (count 0)
        (ret-val)
        (tlist)
        (rlist)
        (token-info)
        (last-token-info)
        (token-type)
        (start-point)
        (end-point)
        (token)
        (nsname)
        (nslist)
        (nsnames)
        (looking)
        (prefix)
        (point-pair)
        (namespace-info)
        )
    (save-excursion
      (setq tlist (shu-cpp-token-first-non-comment token-list))
      (while tlist
        (setq token-info (car tlist))
        (setq count (1+ count))
        (princ (format "%d: %s\n" count (shu-cpp-token-string-token-info token-info)) gb)
        (setq token-type (shu-cpp-token-extract-type token-info))
        (princ (format "      token-type: %d\n" token-type) gb)
        (when (= token-type shu-cpp-token-type-kw)
          (setq token (shu-cpp-token-extract-token token-info))
          (princ (format "      token: \"%s\"\n" token) gb)
          (when (string= token "using")
            (princ "   found using\n" gb)
            (setq start-point (shu-cpp-token-extract-spoint token-info))
            (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-namespace-match-list tlist))
            (setq rlist (cdr ret-val))
            (when rlist
              (princ (format "rlist(%d): " (length rlist)) gb) (princ rlist gb) (princ "\n" gb)
              (setq looking t)
              (while (and rlist looking)
                (setq token-info (car rlist))
                (setq token-type (shu-cpp-token-extract-type token-info))
                (cond
                 ((= token-type shu-cpp-token-type-uq)
                  (setq token (shu-cpp-token-extract-token token-info))
                  (push token nsnames)
                  (princ "nsnames[a]: " gb) (princ nsnames gb) (princ "\n" gb)
                  )
                 ((= token-type shu-cpp-token-type-op)
                  (setq token (shu-cpp-token-extract-token token-info))
                  (when (string= token ";")
                    (setq end-point (shu-cpp-token-extract-epoint token-info))
                    (setq looking nil)
                    )
                  )
                 )
                (when looking
                  (setq rlist (cdr rlist))
                  )
                )
              (setq nsnames (nreverse nsnames))
              (princ "nsnames[b]: " gb) (princ nsnames gb) (princ "\n" gb)
              (setq prefix "")
              (setq nsname "")
              (while nsnames
                (setq nsname (concat nsname prefix (car nsnames)))
                (setq prefix "::")
                (setq nsnames (cdr nsnames))
                )
              )
            (setq namespace-info (shu-cpp-make-namespace-info nsname start-point end-point))
            (push namespace-info nslist)
            )
          )
        (setq tlist (shu-cpp-token-next-non-comment tlist))
        )
      )
    nslist
    ))


;;
;;  shu-cpp-extract-namespace-name
;;
(defsubst shu-cpp-extract-namespace-name (namespace-info)
  "Return the name of a namespace from an instance of namespace-info."
  (car namespace-info)
  )



;;
;;  shu-cpp-make-namespace-info
;;
(defsubst shu-cpp-make-namespace-info (name start-point end-point)
  "Create a namespace-info from the NAME, START-POINT, and END-POINT."
  (let ((point-pair))
    (setq point-pair (cons start-point end-point))
    (cons name point-pair)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;
;;  shu-test-shu-cpp-extract-namespace-name
;;
(ert-deftest shu-test-shu-cpp-extract-namespace-name ()
  (let (
        (name "Freddy")
        (start-point 1023)
        (end-point 1044)
        (namespace-info)
        (actual-name)
        )
    (setq namespace-info (shu-cpp-make-namespace-info name start-point end-point))
    (setq actual-name (shu-cpp-extract-namespace-name namespace-info))
    (should actual-name)
    (should (stringp actual-name))
    (should (string= name actual-name))
    ))




;;
;;  shu-test-shu-cpp-match-find-using-1
;;
(ert-deftest shu-test-shu-cpp-match-find-using-1 ()
  (let ((token-list)
        (token-info)
        (ret)
        (tlist)
        (count 0)
        (data
         (concat
          "// Hi there\n"
          "using namespace ::std;\n"
          "// Nother comment\n")))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (setq ret (shu-cpp-match-find-using token-list))
      (should ret)
      (should (listp ret)))
    ))


;;; shu-cpp-namespace.el ends here
