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
;;
;; If op-code indicates that this is a side list, then we have the structure
;; shown below, with side-list pointing to the first match-info in the side
;; list.
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
;;        |       +-----> side-list
;;        |
;;        +-------------> op-code
;;




(defconst shu-cpp-token-match-type-skip 1
  "The match type constant that indicates skip one input cell.")


(defconst shu-cpp-token-match-type-same 2
  "The match type constant that indicates that the token type and token value
must both match.")


(defconst shu-cpp-token-match-type-same-rx 3
  "The match type constant that indicates that the token type must match and
the token value must staisify the regular expression for a C++ variable name.")


(defconst shu-cpp-token-match-type-non-loop-max 3
  "The maximum match type value that does not indicate a side loop.")


(defconst shu-cpp-token-match-type-side-loop 4
  "The match side constant that indicates a looping side list.  The token list
must match the side list zero or more times.  If the first item in the list does
not match, this is considered a success.  If the first item matches, then all
items in the side list must match.  If all items in the side list match, we go
back to the top of the side list and try again until we find a token that does
not match the first item in the sice list.  The matchis considered a failure
only of there is a partial match between the tokens and the side list.")

(defconst shu-cpp-token-match-type-side-choose 5
  "The match side constant that indicates a choice.  The match is considered a
success if any one item in the side list matches the current token.")




;;
;;  shu-cpp-make-match-info
;;
(defun shu-cpp-make-match-info (op-code match-eval-func match-ret-ind
                                        match-token-type match-token-value)
  "Return a match-info structure from the given arguments"
  (cons op-code
        (cons
         (cons match-ret-ind match-eval-func)
         (cons match-token-type match-token-value)))
  )


;;
;;  shu-cpp-make-match-side-list
;;
(defun shu-cpp-make-match-side-list (op-code match-list)
  "Return a match-info structure from the given arguments that represents a
side list."
  (cons op-code match-list)
  )



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
;;  shu-cpp-match-extract-op-code
;;
(defsubst shu-cpp-match-extract-op-code (match-info)
  "Return the op code from the match-info."
  (car match-info)
  )


;;
;;  shu-cpp-match-extract-side-list
;;
(defsubst shu-cpp-match-extract-side-list (match-info)
  "Return the op code from the match-info."
  (cdr match-info)
  )



;;
;;  shu-cpp-match-is-side-list
;;
(defsubst shu-cpp-match-is-side-list (op-code)
  "Return true if the OP-CODE represents a side list operation."
    (> op-code shu-cpp-token-match-type-non-loop-max)
    )


;;
;;  shu-cpp-match-extract-type
;;
(defsubst shu-cpp-match-extract-type (match-info)
  "Return the token type from the match-info."
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
  "Return the token from the match-info."
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
;; TODO: Find some way to pass back the pointer to the next token
;;       to be scanned.  This is the first token not examined after
;;       a successful match.  Everything up to here has been matched.
;;       No need to look at it again.
;;
;;       Need to have an entry in the match list that is the "name"
;;       of the list.  Pass back the "name" of the matched list as
;;       well so that the caller can tell what was matched.  This
;;       is better than passing back the list index because that
;;       will change if you add a new list to anywhere but the
;;       end.  Depending on what is being matched, it may not be
;;       possible to add it at the end.
;;
;;       If there ia a match-list op code that simply says "this
;;       is a name to be passed back," you could consider passing
;;       back a list of names and even of their corresponding tokens.
;;
;;       e.g., Entry 2 in a match list matches a variable name.
;;       Entry 3 in the match list might be a name called "variable"
;;       to which the previous token is attached.
;;
;;       Perhaps the simplest thing to do would be to add a "name"
;;       field to each entry in the match list and pass back to
;;       the caller a pointer to the list that actually matched.
;;
;;       You can have a named list by simply naming the first entry
;;       or you can choose to have other names in the list.
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
;;  shu-cpp-match-or-list
;;
(defun shu-cpp-match-or-list (rlist token-list match-info)
  "RLIST points to the current return value list, if any.  TOKEN-LIST points to
the next token-info to match.  MATCH-INFO is the head of the side list with
which to match.  The match succeeds if the first token-info in TOKEN-LIST
matches any of the match-info members of MATCH-INFO.  If the match fails, return
nil.  If the match succeeds, return a cons cell pointing to two items.  The car
is the next token-info in TOKEN-LIST.  The cdr is the return list, RLIST.  RLIST
remains unchanged if the match-info that matched did not specify that the
matched token-info was to be returned."
  (let ((token-info (car token-list))
        (side-list (shu-cpp-match-extract-side-list match-info))
        (looking t)
        (op-code)
        (match-eval-func)
        (match-ret-ind)
        (match-token-type)
        (match-token-value)
        (did-match)
        (ret-val))
    (while (and looking side-list)
      (setq match-info (car side-list))
      (shu-cpp-match-extract-info match-info op-code match-eval-func
                                  match-ret-ind match-token-type match-token-value)
      (setq did-match (funcall match-eval-func match-info token-info))
      (when did-match
          (setq looking nil)
        (when match-ret-ind
          (push token-info rlist)))
      (when looking
        (if (not side-list)
            (setq looking nil)
          (setq side-list (cdr side-list)))))
    (when did-match
      (setq token-list (cdr token-list))
      (setq ret-val (cons token-list rlist)))
    ret-val
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

;;
;;  shu-cpp-brace-colon-or-list
;;
(defconst shu-cpp-brace-colon-or-list
   (list  ;; operator "{"
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



;;
;; Names for rmv-using
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
;;        |       +-----> List of unqualified class names
;;        |
;;        +-------------> namespace names
;;
;;
;;  namespace-names:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Replacement name
;;        |
;;        +-------------> Using name
;;
;;
;;  using name is the name specified on "using namespace"
;;
;;  replacmenet name is the one that is added to the unqualified class name
;;
;; So there might be a using name of abcdef::mumble
;;
;; And a replacement name of mumble because the code was about to be
;; enclosed in the namespace abcdef, so you would only have to qualify
;; the class names with mumble.
;;
;;
;;


;;; shu-cpp-match.el ends here
