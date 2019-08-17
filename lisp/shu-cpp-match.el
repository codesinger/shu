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

;;
;; The functions in shu-cpp-token.el can scan a section of C++ source code and
;; turn it into a list of tokens.  Each token has a type (comment, string,
;; keyword, operator or unquoted token) and a value, which is the token itself.
;; Each token also contains its start and end position within the file.
;;
;; The functions in this file, shu-cpp-match.el, allow one to use data
;; structures to describe patterns to be found in a list of tokens.
;;
;; The simplest data structure consists of a list of match items that must
;; exactly match a list of tokens.  If you want to find something that looks
;; like
;;
;;       pointer->thing
;;
;; you put together a list of three match items.  The first specifies a regular
;; expression that can match a C++ name.  The second is an exact match for the
;; operator "->", and the third is another regular expression that can match a
;; C++ name.
;;
;; You can also search for more than one pattern at a time by supplying a list
;; of lists of match items.  Suppose you want to search for an occurrence of a
;; "using namespace" directive.  You might have one list that matches "using
;; namespace name;", another list that matches "using namespace ::name;", and
;; another list that matches "using namespace component::name";.
;;
;; If you try to match this set of three lists against the following string
;;
;;       using namespace thing::Bob
;;
;; The three lists are evaluated as follows:
;;
;; The first list matches "using," "namespace," "thing," but fails when it does
;; not find a semi-colon following "thing."
;;
;; The second list matches "using," "namespace," but fails when it does not find
;; an operator "::".
;;
;; The third list matches "using," "namespace," "thing," "::," "Bob," and ";".
;;
;; With one function call, you have found a reasonably complex pattern.  The
;; tokens scanned to not include comments.  This means that the above example
;; would have worked identically on a list of tokens derived from
;;
;;       using /* Hello */ namespace
;;       // Something here
;;       thing /* How are you? */ :: Bob ;
;;
;; A list of match items can also include a side list.  A side list is another
;; list of match items that is to be matched.  There are different types of side
;; lists.  One of them is a repeating side list.  A repeating side list matches
;; zero or more occurrences of a list.
;;
;; In the above example, we matched the name thing::Bob by having three match
;; items.  But what if we want to match an arbitrary nesting of namespaces, such
;; as
;;
;;       thing::Bob::Fred::Ted
;;
;; One way to do this is with a repeating side list.
;;
;; This list of tokens above could be matched by a single match item followed by
;; a repeating side list.  The first item in the list is a regular expression
;; match for a C++ name.  The second item in the list is a repeating side list,
;; which contains two items, the first of which is an exact match for operator
;; "::", and the second of which is a regular expression that matches a C++
;; name.
;;
;; The match would work as follows:
;;
;; The first match item would match "thing".  Then the repeating side list would
;; match "::," "Bob," "::," "Fred," "::," and "Ted."
;;
;; On return from a successful match, how do you know what was matched?  Each
;; match item can specify that when the item is matched, the matched item is to
;; be added to a list of items to be returned to the caller.
;;
;; Let us return to our original example in which we had three lists, the last
;; of which finally matched.
;;
;;                 *       *     *
;;       using namespace thing::Bob
;;
;; An asterisk has been placed over each item that is marked to be returned to
;; the caller.  At the end of the match, the matching function would return the
;; list
;;
;;       namespace
;;       thing
;;       Bob
;;
;; Now the caller knows what was matched and has a copy of the matched tokens.
;;
;; In our example of matching a nested namespace name, the function might return
;;
;;       thing
;;       Bob
;;       Fred
;;       Ted
;;
;; Now the caller can tell from the length of the list, how deeply nested the
;; namespace name is.
;;
;; Note that since the matched tokens are pushed onto the list, the list is
;; actually returned in reverse order, which the caller can reverse with
;; nreverse.
;;
;; Each matching function also accepts a return list to which it will add newly
;; matched items.  The caller can then match several patterns that build up an
;; ever expanding return list of tokens.  Only the caller knows when it makes
;; sense to reverse the list and to then start processing the reversed list.
;;
;; A final example is the list of match items that is actually used elsewhere to
;; find occurrences of using namespace directives.  It uses another type of side
;; list, which is a list of lists.  This is an illustration of that match
;; structure:
;;
;;
;;
;;                                   using
;;                                     |
;;                                     V
;;                                 namespace
;;                                     |
;;                                     |
;;           +-------------------------+-------------------------+
;;           |                         |                         |
;;           |                         |                         |
;;           V                         V                         V
;;         <name>                     ::                       <name>
;;           |                         |                         |
;;           |                         |                         |
;;           |                         V                         V
;;           |                       <name>           loop of :: followed by <name>
;;           |                         |                         |
;;           |                         |                         |
;;           V                         V                         V
;;           ;                         ;                         ;
;;
;; This is how the above match structure would match
;;
;;       using namespace thing::Bob::Ted;
;;
;; The first two tokens "using" and "namespace" are matched exactly.  The we
;; come to three lists to be evaluated.  The first one matches "thing" but fails
;; to match the ";".  The second fails trying to match "::".  The third matches
;; "thing" and then the repeating side list matches "::," "Bob", "::," and
;; "Ted."  The repeating side list stops the matching when it encounters the
;; terminating semi-colon, and then the next match item in the list matches the
;; terminating semi-colon.
;;
;; The returned list would be
;;
;;       using
;;       namespace
;;       thing
;;       Bob
;;       Ted
;;
;; You can look in the unit tests and in other code that uses the matching code
;; here for more examples of the power of match lists.
;;

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
;; op-code           - Says what the matching operation is
;;                     This could be a single comparison or it could be a
;;                     side-list to be evaluated.
;; match-eval-func   - This id thr function to call do tbe evaluation
;; match-ret-ind     - If true, return this token-info as the return value
;;                     iff the evaluation succeeds
;; match-token-type  - Is a token type to compare against the token-type from
;;                     the token-info
;; match-token-value - Is a possible value or regular expresson to be compared
;;                     with or applied to the token-value from the token-info
;;
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
;;        |       +-----> match-side
;;        |
;;        +-------------> op-code
;;
;;
;;
;;  match-side
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> side-list
;;        |
;;        +-------------> side-parameter
;;
;;
;;  side-parameter is whatever the side list requires, but is currently unused.
;;  It may be used by something like shu-cpp-match-repeat-list to indicate a
;;  required minimum number of matches.  Right now that function succceeds if there
;;  are zero or more matches.
;;



;;
;;   op-codes
;;
(defconst shu-cpp-token-match-type-skip 1
  "The match type constant that indicates skip one input cell.")


(defconst shu-cpp-token-match-type-same 2
  "The match type constant that indicates that the token type and token value
must both match.")


(defconst shu-cpp-token-match-type-same-rx 3
  "The match type constant that indicates that the token type must match and
the token value must satisfy the regular expression for a C++ variable name.")


(defconst shu-cpp-token-match-type-non-loop-max 3
  "The maximum match type value that does not indicate a side loop.")


(defconst shu-cpp-token-match-type-side-loop 4
  "The match side constant that indicates a looping side list.  The token list
must match the side list zero or more times.  If the first item in the list does
not match, this is considered a success.  If the first item matches, then all
items in the side list must match.  If all items in the side list match, we go
back to the top of the side list and try again until we find a token that does
not match the first item in the side list.  The match is considered a failure
only of there is a partial match between the tokens and the side list.")

(defconst shu-cpp-token-match-type-side-choose 5
  "The match side constant that indicates a choice.  The match is considered a
success if any one item in the side list matches the current token.")

(defconst shu-cpp-token-match-type-side-many 6
  "The match side constant that indicates a choice among multiple lists.
This does a recursive call to shu-cpp-match-tokens.")



;;
;;  shu-cpp-match-op-code-name
;;
(defun shu-cpp-match-op-code-name (op-code)
  "Return the name of an op-code."
  (let ((op-code-name "**unknown** (nil)"))
    (when op-code
      (if (not (numberp op-code))
          (setq op-code-name "**unknown** (Not a number)")
        (setq op-code-name (format "**unknown** (%d)" op-code))
        (cond
         ((= op-code shu-cpp-token-match-type-skip)
          (setq op-code-name "skip"))
         ((= op-code shu-cpp-token-match-type-same)
          (setq op-code-name "match-same"))
         ((= op-code shu-cpp-token-match-type-same-rx)
          (setq op-code-name "match-rx"))
         ((= op-code shu-cpp-token-match-type-side-loop)
          (setq op-code-name "side-loop"))
         ((= shu-cpp-token-match-type-side-choose)
          (setq op-code-name "side-choose"))
         ((= shu-cpp-token-match-type-side-many)
          (setq op-code-name "side-many"))
         )))
    op-code-name
    ))

;;
;;  shu-cpp-side-list-functions
;;
(defconst shu-cpp-side-list-functions
  (list
   (cons shu-cpp-token-match-type-side-loop 'shu-cpp-match-repeat-list)
   (cons shu-cpp-token-match-type-side-choose 'shu-cpp-match-or-list)
   (cons shu-cpp-token-match-type-side-many 'shu-cpp-match-many-list))
  "A-list that maps a side list op-code to the function that implements it.")




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
(defun shu-cpp-make-match-side-list (op-code match-list &optional side-parameter)
  "Return a match-info structure from the given arguments that represents a
side list."
  (let (
        (match-side (cons side-parameter match-list))
        )
    (cons op-code match-side)
    ))



;;
;;  shu-cpp-match-extract-info
;;
(defmacro shu-cpp-match-extract-info (match-info op-code match-eval-func
                                                 match-ret-ind match-token-type match-token-value)
  "Extract the information out of a match-info"
  (let ((tmatch-ext (make-symbol "match-ext"))
        (tmatch-func (make-symbol "match-func"))
        (tmatch-type (make-symbol "match-type")))
    `(let ((,tmatch-ext)
           (,tmatch-func)
           (,tmatch-type))
       (setq ,op-code (car ,match-info))
       (setq ,tmatch-ext (cdr ,match-info))
       (setq ,tmatch-func (car ,tmatch-ext))
       (setq ,tmatch-type (cdr ,tmatch-ext))
       (setq ,match-eval-func (cdr ,tmatch-func))
       (setq ,match-ret-ind (car ,tmatch-func))
       (setq ,match-token-type (car ,tmatch-type))
       (setq ,match-token-value (cdr ,tmatch-type)))
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
(defmacro shu-cpp-match-extract-side-list (match-info op-code side-list side-parameter)
  "Extract the side-list information out of a match-info that represents a side-list.."
  (let ((tmatch-side (make-symbol "match-side")))
    `(let ((,tmatch-side))
       (setq ,op-code (car ,match-info))
       (setq ,tmatch-side (cdr ,match-info))
       (setq ,side-parameter (car ,tmatch-side))
       (setq ,side-list (cdr ,tmatch-side)))
    ))



;;
;;  shu-cpp-match-extract-side-list-only
;;
(defun shu-cpp-match-extract-side-list-only (match-info)
  "Extract only the side list from the match info.  This is in contract to
shu-cpp-match-extract-side-list, which extracts all of the properties of a
side list."
  (cddr match-info)
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
  (let ((match-ext)
        (match-type))
    (setq match-ext (cdr match-info))
    (setq match-type (cdr match-ext))
    (car match-type)
    ))


;;
;;  shu-cpp-match-extract-token
;;
(defsubst shu-cpp-match-extract-token (match-info)
  "Return the token from the match-info."
  (let ((match-ext)
        (match-type))
    (setq match-ext (cdr match-info))
    (setq match-type (cdr match-ext))
    (cdr match-type)
    ))




;;
;;  shu-cpp-all-search-match-tokens
;;
(defun shu-cpp-all-search-match-tokens (rlist match-list token-list)
  "Repeatedly call shu-cpp-search-match-tokens until there are no remaining tokens
to match.  If the return value is nil, there were no tokens found to match.  If
the return value is non-nil, its RLIST is the list of all of the returned tokens
from all of the matches."
  (let ((tlist token-list)
        (rlist)
        (ret-val)
        (pret-val)
        (something t))
    (while something
      (setq ret-val (shu-cpp-search-match-tokens rlist match-list tlist))
      (if (not ret-val)
          (setq something nil)
        (setq pret-val ret-val)
        (setq tlist (car ret-val))
        (setq rlist (cdr ret-val))))
    pret-val
    ))



;;
;;  shu-cpp-search-match-tokens
;;
(defun shu-cpp-search-match-tokens (rlist match-list token-list)
  "A function that advances through the TOKEN-LIST until the first item in the
single MATCH-LIST matches the token.  If that happens, try to match the whole
list.  If the whole list is matched, return.  If the whole list does not match,
restore the original RLIST and TOKEN-LIST and continue.  Return only when the
whole list is matched or the TOKEN-LIST is exhausted.

This could be extended to multiple lists by having the search part check for a
match with the head of any of the lists in order.  When the head of one list
matches, pursue that list.  If the list match fails, move to the next list.
When no list head or entire list matches the current token, then move to the
next token."
  (interactive)
  (let ((something t)
        (orig-rlist rlist)
        (tlist token-list)
        (slist)
        (mlist match-list)
        (advance-tlist)
        (match-info)
        (token-info)
        (op-code)
        (match-eval-func)
        (match-ret-ind)
        (match-token-type)
        (match-token-value)
        (did-match)
        (inner-ret)
        (ret-val)
        (nret-val)
        (lcount 0))
    (setq match-info (car mlist))
    (shu-cpp-match-extract-info match-info op-code match-eval-func
                                match-ret-ind match-token-type match-token-value)
    (while (and something tlist)
      (setq lcount (1+ lcount))
      (when (= lcount 9000)
        (setq something nil))
      (setq advance-tlist t)
      (setq token-info (car tlist))
      (setq did-match (funcall match-eval-func match-info token-info))
      (if did-match
          (progn
            (setq slist tlist)
            (setq inner-ret (shu-cpp-internal-sub-match-tokens rlist mlist tlist))
            (setq nret-val (cdr inner-ret))
            (setq mlist (car inner-ret))
            (setq tlist (car nret-val))
            (setq rlist (cdr nret-val))
            (if mlist
                (progn
                  (setq mlist match-list)
                  (setq rlist orig-rlist)
                  (setq tlist slist)
                  (setq advance-tlist t)
                  )
              (setq advance-tlist nil)
              (setq ret-val (cons tlist rlist))
              (setq something nil))))
      (when (and tlist advance-tlist)
        (setq tlist (shu-cpp-token-next-non-comment tlist))))
    ret-val
    ))



;;
;;  shu-cpp-match-tokens
;;
(defun shu-cpp-match-tokens (rlist match-lists token-list)
  "MATCH-LISTS is a list of match lists.  TOKEN-LIST is a list of tokens.  RLIST
is either nil or an existing list of returned tokens on which to build.  For
each match-list in MATCH-LISTS, try to match every element of the match list to
the token list.  if a match fails or if you reach the end of the token list
before reaching the end of the match list, move to the next match list and try
again.  if all elements of a match list match the tokens in the token list, stop
the matching process and return (pushed onto RLIST) a list which consists of
matched tokens whose corresponding entry in the match list indicated that the
matched token was to be added to the list to be returned."
  (let ((outer-done)
        (mlist)
        (tlist)
        (orig-rlist rlist)
        (lcount 0)
        (mcount 0)
        (ret-val)
        (nret-val)
        (inner-ret))
    (while (and match-lists (not outer-done))
      (setq lcount (1+ lcount))
      (setq mcount 0)
      (setq mlist (car match-lists))
      (setq tlist token-list)
      (setq rlist orig-rlist)
      (setq inner-ret (shu-cpp-internal-sub-match-tokens rlist mlist tlist))
      (setq nret-val (cdr inner-ret))
      (setq mlist (car inner-ret))
      (setq tlist (car nret-val))
      (setq rlist (cdr nret-val))
      (if mlist
          (progn
            (setq match-lists (cdr match-lists)))
        (setq outer-done t)
        (setq token-list tlist)
        (setq ret-val (cons token-list rlist))))
    ret-val
    ))


;;
;;  shu-cpp-internal-sub-match-tokens
;;
;; This function has to return three values
;;
;;   mlist
;;   rlist
;;   token-list
;;
;; It returns a single cons cell leading to the three values
;; as follows:
;;
;;
;;  inner-ret
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> ret-val
;;        |
;;        +-------------> mlist
;;
;;
;;
;;
;;  ret-val
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> rlist
;;        |
;;        +-------------> token-list
;;
;;
;;
(defun shu-cpp-internal-sub-match-tokens (rlist mlist tlist)
  "Do the matching for one list only."
  (interactive)
  (let ((inner-done)
        (mcount 0)
        (match-info)
        (token-info)
        (op-code)
        (match-eval-func)
        (match-ret-ind)
        (match-token-type)
        (match-token-value)
        (advance-tlist)
        (ret-val)
        (inner-ret)
        (inner-done)
        (token-list)
        (did-match))
    (while (and tlist mlist (not inner-done))
      (setq mcount (1+ mcount))
      (setq match-info (car mlist))
      (setq token-info (car tlist))
      (shu-cpp-match-extract-info match-info op-code match-eval-func
                                  match-ret-ind match-token-type match-token-value)
      (setq advance-tlist t)
      (when  (/= op-code shu-cpp-token-match-type-skip)
        (if (> op-code shu-cpp-token-match-type-non-loop-max)
            (progn
              (setq ret-val (shu-cpp-match-evaluate-side-list op-code rlist tlist match-info))
              (if (not ret-val)
                  (setq inner-done t)
                (setq token-list (car ret-val))
                (setq tlist token-list)
                (setq advance-tlist nil)
                (setq rlist (cdr ret-val))))
          (setq did-match (funcall match-eval-func match-info token-info))
          (if (not did-match)
              (progn
                (setq inner-done t)
                (setq ret-val nil))
            (setq advance-tlist t)
            (when match-ret-ind
              (push token-info rlist)))))
      (when (not inner-done)
        (setq mlist (cdr mlist))
        (when (and tlist advance-tlist)
          (setq tlist (shu-cpp-token-next-non-comment tlist)))))
    (setq ret-val (cons tlist rlist))
    (setq inner-ret (cons mlist ret-val))
    inner-ret
    ))




;;
;;  shu-cpp-match-evaluate-side-list
;;
(defun shu-cpp-match-evaluate-side-list (op-code rlist token-list match-info)
  "Evaluate a side list in a match list.  Use the op-code in  the match item to
find the function that should evaluate the side list."
  (let ((assoc-item)
        (loop-eval-func)
        (ret-val)
        (new-token-list)
        (new-rlist))
    (setq assoc-item (assoc op-code shu-cpp-side-list-functions))
    (when assoc-item
      (setq loop-eval-func (cdr assoc-item))
      (setq ret-val (funcall loop-eval-func rlist token-list match-info))
      (when ret-val
        (setq new-token-list (car ret-val))
        (setq new-rlist (cdr ret-val))
        (setq ret-val (cons new-token-list new-rlist))))
    ret-val
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
        (side-list (shu-cpp-match-extract-side-list-only match-info))
        (new-rlist rlist)
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
          (push token-info new-rlist)))
      (when looking
        (if (not side-list)
            (setq looking nil)
          (setq side-list (cdr side-list)))))
    (when did-match
      (setq token-list (cdr token-list))
      (setq ret-val (cons token-list new-rlist))
      )
    ret-val
    ))



;;
;;  shu-cpp-match-many-list
;;
(defun shu-cpp-match-many-list (rlist token-list match-info)
  "Do a recursive call to shu-cpp-match-tokens."
  (let ((match-lists (shu-cpp-match-extract-side-list-only match-info))
        (ret-val))
    (setq ret-val (shu-cpp-match-tokens rlist match-lists token-list))
    ret-val
    ))



;;
;;  shu-cpp-match-repeat-list
;;
;; Two termination conditions
;;
;; 1. shu-cpp-match-repeat-sub-list returns nil
;; 2. shu-cpp-match-repeat-sub-list returns unaltered rlist / token-list
;;
;; In both cases, the ret-val from shu-cpp-match-repeat-sub-list
;; is the one to return to the caller.

;;
(defun shu-cpp-match-repeat-list (rlist token-list match-info)
  "RLIST points to the current return value list, if any.  TOKEN-LIST points to
the next token-info to match.  MATCH-INFO is the head of the side list with
which to match.  The match succeeds if the token-infos in TOKEN-LIST match all
of the match-infos in MATCH-LIST zero or more times.  The token-infos are
matched repeatedly against the match-infos.  If there is a failure matching the
first match-info, the match is successful.  If there is a failure matching any
other match-info, the match fails.

This is useful when matching repeating but optional patterns.  For example, a
C++ name could be any of the following:

     a
     a::b
     a::b::c

You can match this with a match list that requires an unquoted token that
matches a C++ name, followed by a side list looking for operator \"::\" followed
by an unquoted token.  If there is no match, then you have an unqualified name.
If it matches once, you have a name with one level of qualification.  But if it
fails in the middle, then you have found something that looks like \"a::\",
which is not a valid C++ name."
  (let ((match-list (shu-cpp-match-extract-side-list-only match-info))
        (orig-match-list (shu-cpp-match-extract-side-list-only match-info))
        (orig-token-list)
        (orig-rlist)
        (new-rlist rlist)
        (new-token-list token-list)
        (looking t)
        (ret-val))
    (while looking
      (setq orig-token-list new-token-list)
      (setq orig-rlist new-rlist)
      (setq ret-val (shu-cpp-match-repeat-sub-list new-rlist new-token-list match-list))
      (if (not ret-val)
          (setq looking nil)
        (setq new-token-list (car ret-val))
        (setq new-rlist (cdr ret-val))
        (if (and
             (eq orig-token-list new-token-list)
             (eq orig-rlist new-rlist))
            (setq looking nil)
          (setq match-list orig-match-list))))
    ret-val
    ))



;;
;;  shu-cpp-match-repeat-sub-list
;;
(defun shu-cpp-match-repeat-sub-list (rlist token-list match-list)
  "Go through one iteration of the repeating list.  The iteration is considered
a success if either of the following are true: 1. The first match fails, or
2. All matches succeed.  If all matches succeed, the updated RLIST and
TOKEN-LIST are returned.  If the first match fails, the RLIST and TOKEN-LIST are
returned unaltered.  It is as though no match was ever attempted.  If some match
other than the first fails, nil is returned.  If the TOKEN-LIST is nil on entry,
this is the equivalent of a first match failure."
  (let ((looking t)
        (ret-val (cons token-list rlist))
        (orig-rlist rlist)
        (orig-token-list token-list)
        (mcount 0)
        (match-info)
        (token-info)
        (op-code)
        (match-eval-func)
        (match-ret-ind)
        (match-token-type)
        (match-token-value)
        (did-match))
    (when token-list
      (while looking
        (setq mcount (1+ mcount))
        (setq token-info (car token-list))
        (setq match-info (car match-list))
        (shu-cpp-match-extract-info match-info op-code match-eval-func
                                    match-ret-ind match-token-type match-token-value)
        (setq did-match (funcall match-eval-func match-info token-info))
        (if (not did-match)
            (progn
              (setq looking nil)
              (when (/= mcount 1)
                (setq ret-val nil)))
          (when match-ret-ind
            (push token-info rlist))
          (setq token-list (shu-cpp-token-next-non-comment token-list))
          (setq match-list (cdr match-list))
          (if match-list
              (progn
                (when (not token-list)
                  (setq ret-val nil)
                  (setq looking nil)))
            (setq ret-val (cons token-list rlist))
            (setq looking nil)))))
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
  "Perform a single match operation between an item in a token list and an item
in a match list.  The token types must be the same and the token value in the
match list must be the same as the token value in the token list."
  (let ((match-token-type (shu-cpp-match-extract-type match-info))
        (match-token (shu-cpp-match-extract-token match-info))
        (token-type (shu-cpp-token-extract-type token-info))
        (token (shu-cpp-token-extract-token token-info))
        (did-match))
    (setq did-match (and (= token-type match-token-type)
                         (string= token match-token)))
    did-match
    ))



;;
;;  shu-cpp-token-match-same-rx
;;
(defun shu-cpp-token-match-same-rx (match-info token-info)
  "Perform a single match operation by regular expression between an
item in a token list and an item in a match list.  The token types
must be the same and the regular expression in the match list must
match (via string-match) the token in the token list."
  (let ((match-token-type (shu-cpp-match-extract-type match-info))
        (match-token (shu-cpp-match-extract-token match-info))
        (token-type (shu-cpp-token-extract-type token-info))
        (token (shu-cpp-token-extract-token token-info))
        (did-match))
    (setq did-match (and (= token-type match-token-type)
                         (string-match match-token token)))
    did-match
    ))




;;
;;  shu-cpp-token-show-match-lists
;;
(defun shu-cpp-token-show-match-lists (match-lists &optional title)
  "Show the data in an instance of match-info."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (mlist match-lists)
        (match-list)
        (ln 0)
        (local-title))
    (when title
      (princ (concat title "\n") gb))
    (while mlist
      (setq match-list (car mlist))
      (setq local-title (format "\nlist-%d" ln))
      (shu-cpp-token-show-match-list match-list local-title)
      (setq ln (1+ ln))
      (setq mlist (cdr mlist)))
    ))




;;
;;  shu-cpp-token-show-match-list
;;
(defun shu-cpp-token-show-match-list (match-list &optional title)
  "Show the data in an instance of match-info."
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (mlist match-list)
        (match-info))
    (when title
      (princ (concat title "\n") gb))
    (while mlist
      (setq match-info (car mlist))
      (shu-cpp-token-show-match-info match-info)
      (setq mlist (cdr mlist)))
    ))




;;
;;  shu-cpp-token-show-match-info
;;
(defun shu-cpp-token-show-match-info (match-info)
  "Show the data in an instance of match-info."
  (let ((gb      (get-buffer-create shu-unit-test-buffer)))
    (shu-cpp-token-show-match-info-buffer match-info gb)
    ))




;;
;;  shu-cpp-token-show-match-info-buffer
;;
(defun shu-cpp-token-show-match-info-buffer (match-info gb)
  "Show the data in an instance of match-info."
  (let ((op-code)
        (new-match-info)
        (match-ext)
        (match-func)
        (match-type)
        (match-ret-ind)
        (match-ret-name)
        (match-eval-func)
        (match-token-type)
        (match-token-value)
        (debug-on-error t))
    (if (not match-info)
        (princ "shu-cpp-token-show-match-info: match-info is nil\n" gb)
      (if (not (consp match-info))
          (princ "shu-cpp-token-show-match-info: match-info is not cons\n" gb)
        (setq op-code (car match-info))
        (if (not op-code)
            (princ "shu-cpp-token-show-match-info: op-code is nil\n" gb)
          (if (not (numberp op-code))
              (progn
                (princ "shu-cpp-token-show-match-info: op-code is not a number\n" gb)
                (princ "\n    op-code: " gb)(princ op-code gb)(princ "\n" gb))
            (princ (format "op-code: %d (%s)\n" op-code (shu-cpp-match-op-code-name op-code)) gb)
            (if (> op-code shu-cpp-token-match-type-non-loop-max)
                (progn
                  (setq new-match-info (shu-cpp-match-extract-side-list-only match-info))
                  (shu-cpp-token-show-match-list new-match-info "\n side list"))
              (setq match-ext (cdr match-info))
              (if (not match-ext)
                  (princ "shu-cpp-token-show-match-info: match-ext is nil\n" gb)
                (if (not (consp match-ext))
                    (princ "shu-cpp-token-show-match-info: match-ext is nt cons cell\n" gb)
                  (setq match-func (car match-ext))
                  (setq match-type (cdr match-ext))
                  (if (not match-func)
                      (princ "shu-cpp-token-show-match-info: match-func is nil\n" gb)
                    (if (not match-type)
                        (princ "shu-cpp-token-show-match-info: match-type is nil\n" gb)
                      (if (not (consp match-func))
                          (princ "shu-cpp-token-show-match-info: match-func is not cons cell\n" gb)
                        (setq match-ret-ind (car match-func))
                        (setq match-ret-name "nil")
                        (when match-ret-ind
                          (setq match-ret-name "t"))
                        (princ (concat "match-ret-ind: " match-ret-name "\n") gb)
                        (setq match-eval-func (cdr match-func))
                        (if (not match-eval-func)
                            (princ "shu-cpp-token-show-match-info: match-eval-func is nil\n" gb)
                          (princ "match-eval-func: " gb) (princ match-eval-func gb) (princ "\n" gb)
                          (setq match-token-type (car match-type))
                          (if (not (numberp match-token-type))
                              (princ "shu-cpp-token-show-match-info: match-token-type is not a number\n" gb)
                            (princ (format "match-token-type: %d (%s)\n" match-token-type
                                           (shu-cpp-token-token-type-name match-token-type)) gb)
                            (setq match-token-value (cdr match-type))
                            (if (not (stringp match-token-value))
                                (princ "shu-cpp-token-show-match-info: match-token-value is not string\n" gb)
                              (princ (concat "match-token-value: \"" match-token-value "\"\n") gb))))))))))))))
    ))



;;; shu-cpp-match.el ends here
