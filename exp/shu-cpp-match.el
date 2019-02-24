;;; shu-cpp-match.el --- Shu project code for dealing wth C++ in Emacs
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

;; Functions to match patterns against list of tokens produced by
;; shu-cpp-token.el.

;;; Code:

(provide 'shu-cpp-match)
(require 'shu-cpp-token)


;;
;;  match-list:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Next in list
;;        |
;;        +-------------> match-info
;;
;;
;;
;;
;;  match-info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-ext
;;        |
;;        +-------------> op-code
;;
;;
;;
;;
;;  match-ext
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-type
;;        |
;;        +-------------> match-func
;;
;;
;;
;;
;;  match-func
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-eval-func
;;        |
;;        +-------------> match-ret-ind
;;
;;
;;
;;
;;  match-type
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-token-value
;;        |
;;        +-------------> match-token-type
;;
;;
;;
;; op-code saya what the matching operation is
;; match-eval-func - If non-nil, call this function to do tbe evaluation
;; match-ret-ind - If true, return this token-info as the return value
;;                 iff the evaluation succeeds
;; match-token-type - Is a token type to compare against the token-type from
;;                    the token-info
;; match-token-value - Is a possible value or regular expresson to be compared
;;                     with or applied to the token-value from the token-info
;;
;;




(defconst shu-cpp-token-match-type-skip 1
  "The match type constant that indicates skip one input cell.")


(defconst shu-cpp-token-match-type-same 2
  "The match type constant that indicates that the token type and token value
must both match.")


(defconst shu-cpp-token-match-type-same-rx 3
  "The match type constant that indicates that the token type must match and
the token value must staisify the regular expression for a C++ variable name.")



;;
;;  shu-cpp-make-match-info
;;
(defun shu-cpp-make-match-info (match-info op-code match-eval-func
                                           match-ret-ind match-token-type match-token-value)
  "Doc string."
  (let (
        (match-type)
        (match-func)
        (match-ext)
        )
    (setq match-type (cons match-token-type match-token-value))
    (setq match-func (cons match-ret-ind match-eval-func))
    (setq match-ext (cons match-func match-type))
    (setq match-info (cons op-code match-ext))
    ))



;;
;;  shu-cpp-match-extract-info
;;
(defmacro shu-cpp-match-extract-info (match-info op-code match-eval-func
                                                 match-ret-ind match-token-type match-token-value)
  "Extract the information out of a match-info"
  (let (
        (tmatch-ext (make-symbol "match-ext"))
        (tmatch-func (make-symbol "match-func"))
        (tmatch-type (make-symbol "match-type"))
        )
    `(let (
           (,tmatch-ext)
           (,tmatch-func)
           (,tmatch-type)
           )
       (setq ,op-code (car ,match-info))
       (setq ,tmatch-ext (cdr ,match-info))
       (setq ,tmatch-func (car ,tmatch-ext))
       (setq ,tmatch-type (cdr ,tmatch-ext))
       (setq ,match-eval-func (cdr ,tmatch-func))
       (setq ,match-ret-ind (car ,tmatch-func))
       (setq ,match-token-type (car ,tmatch-type))
       (setq ,match-token-value (cdr ,tmatch-type))
       )
    ))


;;
;;  shu-cpp-match-extract-type
;;
(defsubst shu-cpp-match-extract-type (match-info)
  "Doc string."
  (interactive)
  (let (
        (match-ext)
        (match-type)
        )
    (setq match-ext (cdr match-info))
    (setq match-type (cdr match-ext))
    (car match-type)
    ))


;;
;;  shu-cpp-match-extract-token
;;
(defsubst shu-cpp-match-extract-token (match-info)
  "Doc string."
  (interactive)
  (let (
        (match-ext)
        (match-type)
        )
    (setq match-ext (cdr match-info))
    (setq match-type (cdr match-ext))
    (cdr match-type)
    ))



;;
;;  shu-cpp-match-tokens
;;
(defun shu-cpp-match-tokens (match-lists token-list &optional skip-comments)
  "MATCH-LISTS is a list of match lists.  TOKEN-LIST is a list of tokens.  for
each match-list in MATCH-LISTS, try to match every element of the match list to
the token list.  if a match fails or if you reach the end of the token list
before reaching the end of the match list, move to the next match list and try
again.  if all elements of a match list match the tokens in the token list, stop
the matching process and return a newly constructed list which consists of
matched tokens whose corresponding entry in the match list indicated that
the matched token was to be added to the list."
  (let (
        (gb (get-buffer-create "**boo**"))
        (match-list)
        (match-info)
        (outer-done)
        (inner-done)
        (did-match)
        (mlist)
        (tlist)
        (rlist)
        (token-info)
        (return-value)
        (op-code)
        (match-eval-func)
        (match-ret-ind)
        (match-token-type)
        (match-token-value)
        (next-tlist (if skip-comments 'shu-cpp-token-next-non-comment 'cdr))
        (lcount 0)
        (mcount 0)
        )
    (princ "\n\nshu-cpp-match-tokens:\n" gb)
    (while (and match-lists (not outer-done))
      (setq lcount (1+ lcount))
      (setq mcount 0)
      (setq mlist (car match-lists))
      (setq tlist token-list)
      (setq inner-done nil)
      (setq rlist nil)
      (while (and tlist mlist (not inner-done))
        (setq mcount (1+ mcount))
        (setq match-info (car mlist))
        (setq token-info (car tlist))
        (shu-cpp-match-extract-info match-info op-code match-eval-func
                                    match-ret-ind match-token-type match-token-value)
        (when  (/= op-code shu-cpp-token-match-type-skip)
          (let (
                (token)
                )
            (princ "   token-info: " gb) (princ token-info gb) (princ "\n" gb)
            (setq token (shu-cpp-token-extract-token token-info))
            (princ (format "   %d-%d: token from token-info: \"%s\"\n" lcount mcount token) gb)
            (princ (format "   %d-%d: match-token-value: \"%s\"\n" lcount mcount match-token-value) gb)
            )
          (setq did-match (funcall match-eval-func match-info token-info))
          (princ "   did-match: " gb) (princ did-match gb) (princ "\n" gb)
          (if (not did-match)
              (setq inner-done t)
            (when match-ret-ind
              (push token-info rlist)
              )
            )
          )
        (when (not inner-done)
          (setq mlist (cdr mlist))
          (when tlist
            (setq tlist (funcall next-tlist tlist))
            )
          )
        )
      (if mlist
          (setq match-lists (cdr match-lists))
        (setq outer-done t)
        )
      )
    (nreverse rlist)
    ))


;;
;;  shu-cpp-token-match-skip
;;
(defun shu-cpp-token-match-skip (tlist)
  "Skip one cell in the input list."
  (if tlist
      (cdr tlist)
    tlist)
  )


;;
;;  shu-cpp-token-match-same
;;
(defun shu-cpp-token-match-same (match-info token-info)
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (match-token-type (shu-cpp-match-extract-type match-info))
        (match-token (shu-cpp-match-extract-token match-info))
        (token-type (shu-cpp-token-extract-type token-info))
        (token (shu-cpp-token-extract-token token-info))
        )
    (princ (format "shu-cpp-token-match-same: token-type: %d, match-token-type: %d, token: \"%s\", match-token: \"%s\"\n" token-type match-token-type token match-token) gb)
    (and (= token-type match-token-type)
         (string= token match-token))
    ))


;;
;;  shu-cpp-token-match-same-rx
;;
(defun shu-cpp-token-match-same-rx (match-info token-info)
  "Doc string."
  (interactive)
  (let (
        (match-token-type (shu-cpp-match-extract-type match-info))
        (token-type (shu-cpp-token-extract-type token-info))
        (token (shu-cpp-token-extract-token token-info))
        (rx (concat shu-cpp-name "+"))
        )
    (and (= token-type match-token-type)
         (string-match rx token))
    ))


(defconst shu-cpp-namespace-match-list
  (list
   (list  ;; "using namespace <name>;"
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "using")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "namespace")))
    (cons shu-cpp-token-match-type-same-rx
          (cons
           (cons t 'shu-cpp-token-match-same-rx)
           (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons t 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op ";")))
    )
   (list  ;; "using namespace <name1>::<name2>;"
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "using")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "namespace")))
    (cons shu-cpp-token-match-type-same-rx
          (cons
           (cons t 'shu-cpp-token-match-same-rx)
           (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op "::")))
    (cons shu-cpp-token-match-type-same-rx
          (cons
           (cons t 'shu-cpp-token-match-same-rx)
           (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons t 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op ";")))
    )
   (list  ;; "using namespace <name1>::<name2>::<name3;"
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "using")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "namespace")))
    (cons shu-cpp-token-match-type-same-rx
          (cons
           (cons t 'shu-cpp-token-match-same-rx)
           (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op "::")))
    (cons shu-cpp-token-match-type-same-rx
          (cons
           (cons t 'shu-cpp-token-match-same-rx)
           (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op "::")))
    (cons shu-cpp-token-match-type-same-rx
          (cons
           (cons t 'shu-cpp-token-match-same-rx)
           (cons shu-cpp-token-type-uq (concat shu-cpp-name "+"))))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons t 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op ";")))
    )
   (list  ;;  "using namespace ::std;"
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "using")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "namespace")))
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
           (cons shu-cpp-token-type-op ";"))))
   (list  ;;  "using namespace ::bsl;"
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "using")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "namespace")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons nil 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op "::")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons t 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-uq "bsl")))
    (cons shu-cpp-token-match-type-same
          (cons
           (cons t 'shu-cpp-token-match-same)
           (cons shu-cpp-token-type-op ";")))
    )
   )
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
;;  TODO: This does *not* include the start and end point of the
;;        semi-colon that terminates the "using namespace"
;;        directive.
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
        (when (= token-type shu-cpp-token-type-uq)
          (setq token (shu-cpp-token-extract-token token-info))
          (princ (format "      token: \"%s\"\n" token) gb)
          (when (string= token "using")
            (princ "   found using\n" gb)
            (setq start-point (shu-cpp-token-extract-spoint token-info))
            (setq rlist (shu-cpp-match-tokens shu-cpp-namespace-match-list tlist t))
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



;;
;;  tccc
;;
(ert-deftest tccc ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (nslist)
        (data
         (concat
          "// This is something\n"
          "using namespace std;\n"
          "  using namespace ::bsl;\n"
          "// This is something else\n"
          "    using \n"
          " // Hello\n"
          " namespace /* there*/  whammo;\n"
          " // again\n"
          " using namespace blammo::target;\n"
          "  /*  Comment  */\n"
          ))
        )
    (with-temp-buffer
      (insert data)
      (setq nslist (ccc))
      (princ "nslist: " gb) (princ nslist gb) (princ "\n" gb)
      )
    ))


;;; shu-cpp-match.el ends here
