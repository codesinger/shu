;;; shu-cpp-mch-funs.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
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

;; A collection of useful functions for dealing with C++ code.
;;
;;

;;; Code:


(provide 'shu-cpp-mch-funs)
(require 'shu-cpp-match)




(defconst shu-cpp-mch-colon-name
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
  "A repeating side list to match zero or more intances of {:: <name>}")




;;
;;  shu-cpp-mch-namespace-forms
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
(defconst shu-cpp-mch-namespace-forms
  (list
   (list  ;; "<name>"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";")
    )


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
                              ";")
    )


   (list  ;; "<name> {:: <name>};"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))

    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                  shu-cpp-mch-colon-name)
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";")
    )
   )
  )


;;
;;  shu-cpp-mch-namespace-list
;;
(defconst shu-cpp-mch-namespace-list
  (list
   (list  ;; "using namespace <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                  shu-cpp-mch-namespace-forms)
    )
   )
  )



;;
;;  shu-cpp-mch-namespace-list-single
;;
(defconst shu-cpp-mch-namespace-list-single
  (list  ;; "using namespace <name>;"
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             t shu-cpp-token-type-kw
                             "using")
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             t shu-cpp-token-type-kw
                             "namespace")
   (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                 shu-cpp-mch-namespace-forms)
   )
  )

;;
;;
;;
;;
;;
;;
;;                                                        using
;;                                                          |
;;                                                          |
;;                              +---------------------------+----------------------+
;;                              |                                                  |
;;                              |                                                  |
;;                              V                                                  |
;;                          namespace                                              |
;;                              |                                                  |
;;                              |                                                  |
;;    +-------------------------+-------------------------+                        |
;;    |                         |                         |                        |
;;    |                         |                         |                        |
;;    V                         V                         V                        |
;;  <name>                     ::                       <name>                     |
;;    |                         |                         |                        |
;;    |                         |                         |                        |
;;    |                         V                         V                        |
;;    |                       <name>           loop of :: followed by <name>       |
;;    |                         |                         |                        |
;;    |                         |                         |                        |
;;    V                         V                         V                        |
;;    ;                         ;                         ;                        |
;;                                                                                 |
;;                                                                                 |
;;                                                                                 |
;;                                                                                 |
;;                                                                                 |
;;                                                  +------------------------------+
;;                                                  |
;;                                                  |
;;                                 +----------------+---------------+
;;                                 |                                |
;;                                 |                                |
;;                                 V                                V
;;                               <name>                           <name>
;;                                 |                                |
;;                                 |                                |
;;                                 |                                V
;;                                 |                     loop of :: followed by <name>
;;                                 |                                |
;;                                 |                                |
;;                                 V                                V
;;                                 ;                                ;
;;
;;


;;
(defconst shu-cpp-mch-using-forms
  (list
   (list  ;; "<name>"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";")
    )


   (list  ;; "<name> {:: <name>};"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))

    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                  shu-cpp-mch-colon-name)
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                              ";")
    )
   )
  "These two lists match the form of name that can follow a \"using\" directive
that is not a \"using namespace\" directive.  This is either <name> or
<name>::<name>>, <name>::<name>::<name>, etc.  The first list above matches
<name> followed by semicolon..  The second list matches <name> followed
by a looping side list for zero or more occurrences of \"::\" followed by <name>")






;;
;;  shu-cpp-mch-many-using-list
;;
(defconst shu-cpp-mch-many-using-list
  (list
   (list  ;; "namespace <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                  shu-cpp-mch-namespace-forms)
    )

   (list  ;; "using namespace <name>;"
    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                  shu-cpp-mch-using-forms)
    )

   )
  "These two lists match what may follow the key word \"using\".  The first list
matches the key word \"namespace\" followed by any of the different name types
that may follow \"using namespace\".  The second list matches any of the name
forms that may following the key word \"using\" when it is not followed by the
key word \"namespace\"." )



;;
;;  shu-cpp-mch-using-list-single
;;
(defconst shu-cpp-mch-using-list-single
  (list  ;; "using namespace <name>;"
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             t shu-cpp-token-type-kw
                             "using")
   (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                 shu-cpp-mch-many-using-list)
   )
  "This is a sigle list that is the top level list for matching anything
that may follow the key word \"using\".")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;
;;  something-or-other
;;
(defun something-or-other (class-list log-buf &optional top-name)
  "Doc string."
  (let (
        (token-list)
        (ret-val)
        (uns-list)
        (un-list)
        (something)
        (proc-classes)
        (proc-rlists)
        (class-ht)
        (count-alist)
        (clist)
        )
    (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (if (not ret-val)
        (progn
          (message "%s" "No using namespace directives found")
          )
      (setq uns-list (car ret-val))
      (setq un-list (cdr ret-val))
      (when uns-list
        (setq something (shu-match-merge-namespaces-with-class-list class-list uns-list log-buf top-name))
        (if (not something)
            (message "%s" "Failure")
          (setq proc-classes (car something))
          (setq proc-rlists (cdr something))
          (setq something (shu-match-add-names-to-class-list un-list proc-classes proc-rlists log-buf top-name))
          (setq proc-classes (car something))
          (setq proc-rlists (cdr something))
          (setq proc-classes (remove-class-duplicates proc-classes log-buf))
          (setq ret-val (shu-match-remove-proc-rlists token-list proc-rlists log-buf))
          (setq token-list (car ret-val))
          (setq proc-rlists (cdr ret-val))
          (shu-match-erase-using proc-rlists log-buf)
          (setq class-ht (shu-match-make-class-hash-table-internal proc-classes log-buf))
          (when class-ht
            (setq clist (shu-match-find-unqualified-class-names class-ht token-list proc-classes log-buf))
            (when clist
              (setq count-alist (shu-match-make-count-alist-from-hash class-ht))
              (shu-match-qualify-class-names class-ht count-alist clist log-buf)
              )
            )
          )
        )
      )
    ))



;;
;;  shu-match-find-unqualified-class-names
;;
(defun shu-match-find-unqualified-class-names (class-ht token-list proc-classes log-buf)
  "Go through all of the tokens looking at any unquoted token that is in the hash
table of unqualified class names.  When an instance of a class name is found,
check to see it it is preceded by \"::\", \".\", or \"->\".  \"::\" indicates
that it is probably qualified.  \".\" or \"->\" indicate that it is probably a
function name.

Check also to see if it is followed by \")\" or \"[\", which probably indicates
that it is a variable name.

Next, check to see if it is preceded by \"#include\".

If it survives all of those checks, it is probably an unqualified class name, in
which case its token-info is pushed onto a list.  The list is the return value
of this function.  Since each token-info is pushed onto the list, the list is
returned in reverse order.  i.e., the last token in the file is the first in the
list.

At this point it is possible to visit each token in the list, which is in
reverse order in the file, look up each token, and insert in front of it its
qualifying namespace."
  (interactive)
  (let ((tlist token-list)
        (token-info)
        (next-token-info)
        (hv)
        (n)
        (last-token "")
        (last-token-type 0)
        (token "")
        (token-type 0)
        (next-token "")
        (next-token-type 0)
        (blocked)
        (clist))
    (while tlist
      (setq blocked nil)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq hv (gethash token class-ht))
        (when hv
          (if
              (and
               (= last-token-type shu-cpp-token-type-op)
               (or
                (string= last-token "::")
                (string= last-token ".")
                (string= last-token "->")
                ))
              (setq blocked t)
            (setq n (shu-cpp-token-next-non-comment tlist))
            (when n
              (setq next-token-info (car n))
              (setq next-token (shu-cpp-token-extract-token token-info))
              (setq next-token-type (shu-cpp-token-extract-type token-info))
              (if
                  (and
                   (= next-token-type shu-cpp-token-type-op)
                   (or
                    (string= next-token "(")
                    (string= next-token "[")
                    ))
                  (setq blocked t)
                (when (shu-match-rmv-might-be-include token-info)
                  (setq blocked t)))))
          (when (not blocked)
            (push token-info clist))))
      (setq last-token token)
      (setq last-token-type token-type)
      (setq tlist (shu-cpp-token-next-non-comment tlist)))
    clist
    ))


;;
;;  shu-match-qualify-class-names
;;
(defun shu-match-qualify-class-names (class-ht count-alist clist log-buf)
  "CLASS-HT is the hash table that maps a class name to its containing namespace
name.  COUNT-ALIST is the alist that counts the number of times each class
name has been qualified by its enclosing namespace.  CLIST is the list of
token-info, each of which represents an unqualified class name.  The list is in
reverse order, which is important.  It means that one can add a qualification to
one class name in the list without changing the location of any other class
names, which are above the current one in the buffer.

This function goes to the position of each unqualified class name, finds its
containing namespace in the hash table, and inserts the containing namespace
followed by \"::\" in front of the unqualified class name.

After it inserts the qualifying namespace, it increments in COUNT-ALIST the number
of times that the class name was explicitly qualified."
  (let ((token-info)
        (token)
        (spoint)
        (hv))
    (while clist
      (setq token-info (car clist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq spoint (shu-cpp-token-extract-spoint token-info))
      (goto-char spoint)
      (setq hv (gethash token class-ht "????"))
      (insert (concat hv "::"))
      (shu-match-increment-class-count count-alist token)
      (setq clist (cdr clist)))
    (shu-match-rmv-show-class-count count-alist class-ht log-buf)
    ))




;;
;;  shu-match-rmv-might-be-include
;;
(defun shu-match-rmv-might-be-include (token-info)
  "Go to the beginning of the line in front of the start point of the
TOKEN-INFO.  Return true if the space between the beginning of the line and the
start point of the TOKEN-INFO contains \"#include\"."
  (let ((spoint (shu-cpp-token-extract-spoint token-info))
        (blocked))
    (save-excursion
      (goto-char spoint)
      (beginning-of-line)
      (when (re-search-forward "#\\s-*include" spoint t)
        (setq blocked t)))
    blocked
    ))




;;
;;  shu-match-erase-using
;;
(defun shu-match-erase-using (proc-rlists log-buf)
  "PROC-RLISTS is the set of rlists we are processing that represent all of the
\"using namespace\" and \"using name\" statements.  This function replaces all
of those statements in the buffer with whitespace.  This is done in order to
preserve the positions of all other items in the buffer."
  (interactive)
  (let ((rlist)
        (ret-val)
        (spoint)
        (epoint)
        (region (buffer-substring-no-properties (point-min) (point-max))))
    (save-excursion
      (while proc-rlists
        (setq rlist (car proc-rlists))
        (setq ret-val (shu-match-get-start-end-pos rlist))
        (setq spoint (car ret-val))
        (setq epoint (cdr ret-val))
        (shu-erase-region spoint (1+ epoint))
        (setq proc-rlists (cdr proc-rlists))))
    (setq region (buffer-substring-no-properties (point-min) (point-max)))
    ))



;;
;;  shu-match-remove-proc-rlists
;;
(defun shu-match-remove-proc-rlists (token-list proc-rlists log-buf)
  "TOKEN-LIST is the original token list.  PROC-RLISTS is the set of rlists that
represents the set of statements we will be processing.  This function removes
from TOKEN-LIST, all of the items that are contained in the rlists in
PROC-RLISTS.  This is because we do not want a subsequent scan of the token list
to include any of the items in the statements we are processing.

The return value from this function is a cons cell whose car is the trimmed
TOKEN-LIST and whose cdr is the sorted PROC-RLISTS, which has been sorted by the
start position of each rlist."
  (let ((tlist token-list)
        (sorted-proc
         (sort proc-rlists
               (lambda(lhs rhs)
                 (let ((lti (car lhs))
                       (rti (car rhs)))
                   (< (shu-cpp-token-extract-spoint lti) (shu-cpp-token-extract-spoint rti))))))
        (sprl)
        (rlist)
        (ret-val)
        (r-spoint)
        (r-epoint)
        (lastp)
        (first1)
        (looking-for-start)
        (token-info)
        (spoint)
        (epoint)
        (looking-for-end))
    (setq sprl sorted-proc)
    (while (and sprl tlist)
      (setq rlist (car sprl))
      (setq ret-val (shu-match-get-start-end-pos rlist))
      (setq r-spoint (car ret-val))
      (setq r-epoint (cdr ret-val))
      (setq first1 nil)
      (setq looking-for-start t)
      (while looking-for-start
        (setq token-info (car tlist))
        (setq spoint (shu-cpp-token-extract-spoint token-info))
        (if (= spoint r-spoint)
            (progn
              (setq looking-for-start nil)
              (when (equal (car tlist) (car token-list))
                (setq first1 t)))
          (setq lastp tlist)
          (setq first1 nil)
          (setq tlist (cdr tlist))))
      ;; lastp is the item whose cdr will be changed
      ;; If first1 is true, head of list will be changed
      ;; tlist points to the start item (which might also be the end)
      (setq looking-for-end t)
      (while looking-for-end
        (setq token-info (car tlist))
        (setq epoint (shu-cpp-token-extract-epoint token-info))
        (if (= epoint r-epoint)
            (progn
              (setq looking-for-end nil)
              (if first1
                  (setq token-list (cdr tlist))
                (setcdr lastp (cdr tlist))
                (setq tlist (cdr tlist))))
          (setq tlist (cdr tlist))))
      (setq sprl (cdr sprl)))
    (cons token-list sorted-proc)
    ))


;;
;;  remove-class-duplicates
;;
(defun remove-class-duplicates (proc-classes log-buf)
  "PROC-CLASSES is the alist of classes that we will process.  The car of each
item is the containing namespace.  The cdr of each item is the list of class
names contained within the namespace.  The original class list may have had
duplicate class names within a given namespace.  We may also have added class
names to a given namespace from processing one or more \"using name\"
statements.

For example, if the lass list contained \"std . set map string\" and we also
processed a \"using std::string\" statement, the code that processed that
statement would have blindly added \"string\" to the list of classes in the
namespace \"std\".  The list of classes for namespace \"std\" would then contain
\"string set map string\".

This function removes any duplicate class names within a given namespace.  This
is necessary because we are about to invert the class list to produce a hash
table in which the key is the class name and the value is the enclosing
namespace name.  This operation will fail if a given namespace contains
duplicate class names."
  (let (
        (pc proc-classes)
        (ce)
        (cl)
        )
    (while pc
      (setq ce (car pc))
      (setq cl (cdr ce))
      (setq cl (delete-dups cl))
      (setcdr ce cl)
      (setq pc (cdr pc))
      )
    proc-classes
    ))


;;
;;  shu-match-add-names-to-class-list
;;
(defun shu-match-add-names-to-class-list (un-list proc-classes proc-rlists log-buf &optional top-name)
  "UN-LIST is the list of rlists that represent the \"using name\" statements.
PROC-CLASSES is the class list we will be using to do the processing.
PROC-RLISTS is the set of rlists that represents the \"using namespace\"
statements.  This function adds the\"using name\" directives, if any, to both
PROC-CLASSES and PROC-RLISTS.  It returns a cons cell in which the car is the
modified PROC-CLASSES and the cdr is the modified P{ROC-RLISTS."
  (let ((rlist)
        (ret-val)
        (ns-name)
        (class-name)
        (ce)
        (x)
        (cl))
    (while un-list
      (setq rlist (car un-list))
      (push rlist proc-rlists)
      (setq ret-val (shu-match-using-string rlist top-name))
      (setq ns-name (car ret-val))
      (setq class-name (cdr ret-val))
      (setq ce (assoc ns-name proc-classes))
      (if (not ce)
          (progn
            (setq x (cons ns-name (list class-name)))
            (push x proc-classes))
        (setq cl (cdr ce))
        (push class-name cl))
      (setq un-list (cdr un-list)))
    (cons proc-classes proc-rlists)
    ))



;;
;;  shu-match-merge-namespaces-with-class-list
;;
(defun shu-match-merge-namespaces-with-class-list (class-list uns-list log-buf &optional top-name)

  "Merge the UNS-LIST with the CLASS-LIST.  Return a cons-cell pointing to two
lists.  The first is the list of classes from the class list that have
corresponding \"using namespace\" directives in the buffer.  The second is the
lists of rlists that represent each using namespace directive that we will
process.
The updated class list will be used to identify class names to be qualified and
the namespaces with which to qualify them..  The rlists representing the \"using
namespace\" statements will be used to remove the \"using namespace\" statements
from the buffer."
  (let (
        (rlist)
        (ns-name)
        (ce)
        (proc-classes)
        (proc-rlists)
        (np-rlists)
        (ret-val)
        (spoint)
        (epoint)
        (ns-code)
          )
    (while uns-list
      (setq rlist (car uns-list))
      (setq ns-name (shu-match-using-namespace-string rlist top-name))
      (setq ce (assoc ns-name class-list))
      (if (not ce)
          (push rlist np-rlists)
        (when (not (assoc ns-name proc-classes))
          (push ce proc-classes)
          (push rlist proc-rlists)
          )
        )
      (setq uns-list (cdr uns-list))
      )
    (when np-rlists
      (setq np-rlists (nreverse np-rlists))
      (while np-rlists
        (setq rlist (car np-rlists))
        (setq ret-val (shu-match-get-start-end-pos rlist t))
        (setq spoint (car ret-val))
        (setq epoint (cdr ret-val))
        (setq ns-code (buffer-substring-no-properties spoint epoint))
        (setq np-rlists (cdr np-rlists))
        )
      )
    (when proc-classes
      (setq proc-classes (nreverse proc-classes))
      )
    (when proc-rlists
      (setq proc-rlists (nreverse proc-rlists))
      )
    (if (and proc-classes proc-rlists)
        (setq ret-val (cons proc-classes proc-rlists))
      (setq ret-val nil)
      )
    ret-val
    ))





;;
;;  shu-match-make-class-hash-table-internal
;;
(defun shu-match-make-class-hash-table-internal (proc-classes log-buf)
  "PROC-CLASSES is the alist of all of the namespaces and classes that we will
process, with the namespace name being the key and a list of class names within the
namespace name as the value.

This function builds a hash table that inverts the
alist.  Each entry in the hash table has a class name as the key with the name
of the enclosing namespace as the value.
If two class names map to the same enclosing namespace name, then there is an
unresolvable ambiguity that must terminate the operation.  If that is the case,
diagnostic messages are placed into the log buffer and a nil value is returned."
  (let ((pc proc-classes)
        (ht)
        (count 0)
        (ce)
        (cl)
        (ns-name)
        (class-name)
        (hv)
        (dup-present))
    (while pc
      (setq ce (car pc))
      (setq cl (cdr ce))
      (setq count (+ count (length cl)))
      (setq pc (cdr pc)))
    (setq ht (make-hash-table :test 'equal :size count))
    (setq pc proc-classes)
    (while pc
      (setq ce (car pc))
      (setq ns-name (car ce))
      (setq cl (cdr ce))
      (while cl
        (setq class-name (car cl))
        (setq hv (gethash class-name ht))
        (if hv
            (progn
              (princ (format "Duplicate namespace for class: %s\n" class-name) log-buf)
              (princ (format "Class '%s' is in both namespace '%s' and '%s'\n" class-name hv ns-name) log-buf)
              (setq dup-present t))
          (puthash class-name ns-name ht))
        (setq cl (cdr cl)))
      (setq pc (cdr pc)))
    (when dup-present
      (setq ht nil))
    ht
    ))


;;
;;  shu-match-make-count-alist-from-hash
;;
(defun shu-match-make-count-alist-from-hash (class-ht)
  "Doc string."
  (interactive)
  (let ((count-alist))
    (maphash (lambda (class-name ns-name)
               (push (cons class-name 0) count-alist))
             class-ht)
    count-alist
    ))



;;
;;  shu-match-increment-class-count
;;
(defun shu-match-increment-class-count (alist class-name)
  "ALIST is an alist whose key is a CLASS-NAME and whose cdr is a count of the
number of times that class name has been found.  This function increments the
count by one."
  (interactive)
  (let ((cv (assoc class-name alist))
        (count))
    (setq count (cdr cv))
    (setq count (1+ count))
    (setcdr cv count)
    ))



;;
;;  shu-match-rmv-show-class-count
;;
(defun shu-match-rmv-show-class-count (count-alist class-ht log-buf)
  "Put into the log buffer the count of class names that were qualified."
  (let ((cp)
        (class-name)
        (ns-name)
        (count)
        (full-name)
        (clist)
        (sum 0)
        (pcount)
        (psum))
    (while count-alist
      (setq cp (car count-alist))
      (setq class-name (car cp))
      (setq count (cdr cp))
      (setq ns-name (gethash class-name class-ht))
      (setq full-name (concat ns-name "::" class-name))
      (push (cons full-name count) clist)
      (setq count-alist (cdr count-alist)))
    (setq clist (sort clist
                      (lambda(lhs rhs)
                        (string< (car lhs) (car rhs))
                        )))
    (while clist
      (setq cp (car clist))
      (setq full-name (car cp))
      (setq count (cdr cp))
      (setq sum (+ sum count))
      (setq pcount (shu-fixed-format-num count 15))
      (princ (concat pcount ": " full-name "\n") log-buf)
      (setq clist (cdr clist)))
    (setq psum (shu-fixed-format-num sum 0))
    (message "%s class names qualified.  See buffer %s" psum (buffer-name log-buf))
    ))



;;
;;  shu-test-shu-match-make-count-alist-from-hash
;;
(ert-deftest shu-test-shu-match-make-count-alist-from-hash ()
  (let ((class-ht (make-hash-table :test 'equal :size 12))
        (count-alist)
        (count)
        (cp1)
        (cp2)
        (cp3)
        (cp4)
        (cp5))
    (puthash "set" "std" class-ht)
    (puthash "string" "std" class-ht)
    (puthash "map" "std" class-ht)
    (puthash "Mumble" "abcde" class-ht)
    (setq count-alist (shu-match-make-count-alist-from-hash class-ht))
    (setq cp1 (assoc "set" count-alist))
    (should cp1)
    (should (consp cp1))
    (setq count (cdr cp1))
    (should (numberp count))
    (should (= count 0))
    (setq cp2 (assoc "string" count-alist))
    (should cp2)
    (should (consp cp2))
    (setq count (cdr cp2))
    (should (numberp count))
    (should (= count 0))
    (setq cp3 (assoc "map" count-alist))
    (should cp3)
    (should (consp cp3))
    (setq count (cdr cp3))
    (should (numberp count))
    (should (= count 0))
    (setq cp4 (assoc "Mumble" count-alist))
    (should cp4)
    (should (consp cp4))
    (setq count (cdr cp4))
    (should (numberp count))
    (should (= count 0))
    (setq cp5 (assoc "Wheeeee!" count-alist))
    (should (not cp5))
    ))



;;
;;  shu-test-shu-match-increment-class-count
;;
(ert-deftest shu-test-shu-match-increment-class-count ()
  (let ((count-alist
         (list
          (cons "set" 0)
          (cons "map" 2)
          (cons "string" 6)
          (cons "Mumble" 1))))
    (shu-match-increment-class-count count-alist "map")
    (setq cp (assoc "map" count-alist))
    (should cp)
    (should (consp cp))
    (setq count (cdr cp))
    (should count)
    (should (numberp count))
    (should (= 3 count))
    ))



;;
;;  shu-test-shu-match-rmv-show-class-count
;;
(ert-deftest shu-test-shu-match-rmv-show-class-count ()
  (let ((count-alist
         (list
          (cons "set" 0)
          (cons "map" 2)
          (cons "string" 6)
          (cons "Mumble" 1)))
        (class-ht (make-hash-table :test 'equal :size 12))
        (expected
         (concat
          "              1: abcde::Mumble\n"
          "              2: std::map\n"
          "              0: std::set\n"
          "              6: std::string\n"))
        (actual))
    (puthash "set" "std" class-ht)
    (puthash "string" "std" class-ht)
    (puthash "map" "std" class-ht)
    (puthash "Mumble" "abcde" class-ht)
    (with-temp-buffer
      (shu-match-rmv-show-class-count count-alist class-ht (current-buffer))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-something-or-other-1
;;
(ert-deftest shu-test-something-or-other-1 ()
  (let (
        (log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using std::string;\n"
          "    using namespace fred; /* Hello */\n"
          "    using abc::std::deque;\n /* Hello */"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          )
         )
        (token-list)
        )
    (with-temp-buffer
      (insert data)
      (something-or-other class-list log-buf)
      )
    ))






;;
;;  shu-test-something-or-other-2
;;
(ert-deftest shu-test-something-or-other-2 ()
  (let (
        (log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using std::deque;\n"
          "    using namespace fred; /* Hello */\n"
          "    using abc::std::string;\n /* Hello */"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          )
         )
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "                          \n"
          "    x = x + 1;\n"
          "                          \n"
          "                     \n"
          "    using namespace fred; /* Hello */\n"
          "                           \n"
          " /* Hello */    /* xxx */\n"
          "    abc::std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          ))
        )
    (with-temp-buffer
      (insert data)
      (something-or-other class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )
    ))





;;
;;  shu-test-something-or-other-3
;;
(ert-deftest shu-test-something-or-other-3 ()
  (let (
        (log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using namespace std;\n"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"      (list
                            "string"
                            "deque"
                            ))
          )
         )
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "                          \n"
          "    x = x + 1;\n"
          "                          \n"
          "                        \n"
          "    /* xxx */\n"
          "    std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          ))
        )
    (with-temp-buffer
      (insert data)
      (something-or-other class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      (princ (concat "\n\n\nsomething-or-other\n" actual) log-buf)
      )
    ))





;;
;;  shu-test-something-or-other-4
;;
(ert-deftest shu-test-something-or-other-4 ()
  (let (
        (log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "    using namespace std;\n"
          "    using abcde::AClass1;\n"
          "    using xyrzk::Xclass2;\n"
          "    using std::string;\n"
          "    using std::deque;\n"
          "    x = x + 1;\n"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"      (list
                            "string"
                            "deque"
                            ))
          )
         )
        (token-list)
        (actual)
        (expected
         (concat
          ))
        )
    (with-temp-buffer
      (insert data)
      (something-or-other class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
;;;      (should (string= expected actual))
      (princ (concat "\n\n\nsomething-or-other\n" actual) log-buf)
      )
    ))



;;
;;  shu-test-remove-class-duplicates
;;
(ert-deftest shu-test-remove-class-duplicates ()
  (let (
        (gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"     (list
                           "string"
                           "string"
                           "set"
                           "set"
                           "map"
                           ))))
        (proc-class)
        (cp1)
        (cp2)
        (cp3)
        (cl)
        )
    (setq proc-class (remove-class-duplicates class-list gb))
    (setq cp1 (assoc "abcde" class-list))
    (should cp1)
    (should (consp cp1))
    (setq cl (cdr cp1))
    (should cl)
    (should (listp cl))
    (should (= 2 (length cl)))
    (setq cp2 (assoc "xyrzk" class-list))
    (should cp2)
    (should (consp cp2))
    (setq cl (cdr cp2))
    (should cl)
    (should (listp cl))
    (should (= 2 (length cl)))
    (setq cp3 (assoc "std" class-list))
    (should cp3)
    (should (consp cp3))
    (setq cl (cdr cp3))
    (should cl)
    (should (listp cl))
    (should (= 3 (length cl)))
    ))


;;
;;  shu-test-shu-match-make-count-alist-from-hash-1
;;
(ert-deftest shu-test-shu-match-make-count-alist-from-hash-1 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"    (list
                          "string"
                          "set"
                          "map"
                          ))))
        (ht)
        (av1)
        (av2)
        (av3)
        (av4)
        (av5)
        (av6)
        (av7)
        (av8)
        (count-alist))
    (setq ht (shu-match-make-class-hash-table-internal class-list gb))
    (should ht)
    (should (hash-table-p ht))
    (setq count-alist (shu-match-make-count-alist-from-hash ht))
    (setq av1 (assoc "AClass1" count-alist))
    (should av1)
    (should (consp av1))
    (should (car av1))
    (should (stringp (car av1)))
    (should (string= "AClass1" (car av1)))
    (should (cdr av1))
    (should (numberp (cdr av1)))
    (should (= 0 (cdr av1)))
    (setq av2 (assoc "AClass2" count-alist))
    (should av2)
    (should (consp av2))
    (should (car av2))
    (should (stringp (car av2)))
    (should (string= "AClass2" (car av2)))
    (should (cdr av2))
    (should (numberp (cdr av2)))
    (should (= 0 (cdr av2)))
    (setq av3 (assoc "Xclass1" count-alist))
    (should av3)
    (should (consp av3))
    (should (car av3))
    (should (stringp (car av3)))
    (should (string= "Xclass1" (car av3)))
    (should (cdr av3))
    (should (numberp (cdr av3)))
    (should (= 0 (cdr av3)))
    (setq av4(assoc "Xclass2" count-alist))
    (should av4)
    (should (consp av4))
    (should (car av4))
    (should (stringp (car av4)))
    (should (string= "Xclass2" (car av4)))
    (should (cdr av4))
    (should (numberp (cdr av4)))
    (should (= 0 (cdr av4)))
    (setq av5 (assoc "string" count-alist))
    (should av5)
    (should (consp av5))
    (should (car av5))
    (should (stringp (car av5)))
    (should (string= "string" (car av5)))
    (should (cdr av5))
    (should (numberp (cdr av5)))
    (should (= 0 (cdr av5)))
    (setq av6 (assoc "set" count-alist))
    (should av6)
    (should (consp av6))
    (should (car av6))
    (should (stringp (car av6)))
    (should (string= "set" (car av6)))
    (should (cdr av6))
    (should (numberp (cdr av6)))
    (should (= 0 (cdr av6)))
    (setq av7 (assoc "map" count-alist))
    (should av7)
    (should (consp av7))
    (should (car av7))
    (should (stringp (car av7)))
    (should (string= "map" (car av7)))
    (should (cdr av7))
    (should (numberp (cdr av7)))
    (should (= 0 (cdr av7)))
    (setq av8 (assoc "Wheeeeeeeee" count-alist))
    (should (not av8))
    ))


;;
;;  shu-test-shu-match-make-class-hash-table-internal-1
;;
(ert-deftest shu-test-shu-match-make-class-hash-table-internal-1 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"    (list
                          "string"
                          "set"
                          "map"
                          ))))
        (ht)
        (hv))
    (setq ht (shu-match-make-class-hash-table-internal class-list gb))
    (should ht)
    (should (hash-table-p ht))
    (setq hv (gethash "AClass1" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "abcde" hv))
    (setq hv (gethash "AClass2" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "abcde" hv))
    (setq hv (gethash "Xclass1" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "xyrzk" hv))
    (setq hv (gethash "Xclass2" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "xyrzk" hv))
    (setq hv (gethash "string" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "std" hv))
    (setq hv (gethash "set" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "std" hv))
    (setq hv (gethash "map" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "std" hv))
    ))




;;
;;  shu-test-shu-match-make-class-hash-table-internal-3
;;
(ert-deftest shu-test-shu-match-make-class-hash-table-internal-3 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "Xclass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "set"
                            "Xclass2"
                            ))
          (cons "std"    (list
                          "string"
                          "set"
                          "map"
                          ))))
        (ht)
        (hv))
    (setq ht (shu-match-make-class-hash-table-internal class-list gb))
    (should (not ht))
    ))



;;
;;  shu-test-shu-match-remove-proc-rlists-1
;;
(ert-deftest shu-test-shu-match-remove-proc-rlists-1 ()
  (let (
        (log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using std::string;\n"
          "    Xclass2     p;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          )
         )
        (top-name "WhammoCorp")
        (token-list)
        (ret-val)
        (token-list)
        (proc-rlists)
        (prl)
        (rlist)
        )
    (with-temp-buffer
      (insert data)
      (setq ret-val (shu-test-shu-setup-proc-rlists-1 class-list log-buf top-name))
      (should ret-val)
      (should (consp ret-val))
      (setq token-list (car ret-val))
      (should token-list)
      (should (listp token-list))
      (setq proc-rlists (cdr ret-val))
      (should proc-rlists)
      (shu-cpp-tokenize-show-list-buffer token-list log-buf "\ntoken-list (setup):")
      (setq prl proc-rlists)
      (while prl
        (setq rlist (car prl))
        (shu-cpp-tokenize-show-list-buffer rlist log-buf "\nrlist (setup):")
        (setq prl (cdr prl))
        )
      (setq ret-val (shu-match-remove-proc-rlists token-list proc-rlists log-buf))
      (should ret-val)
      (should (consp ret-val))
      (setq token-list (car ret-val))
      (should token-list)
      (should (listp token-list))
      (shu-cpp-tokenize-show-list-buffer token-list log-buf "\ntoken-list (setup):")
      )
    ))


;;
;;  shu-test-shu-setup-proc-rlists-1
;;
(defun shu-test-shu-setup-proc-rlists-1 (class-list log-buf &optional top-name)
  "Create from the current buffer, an instance of proc-classes and an instance
of proc-rlist.  Return a cons cell in which the cdr is the instance of
proc-classes and the car is the instance of proc-rlists.

This is used to set up for a unit test that needs these two lists to
be in place."
  (let ((token-list)
        (ret-val)
        (something)
        (uns-list)
        (un-list)
        (proc-rlists)
        (proc-classes))
    (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (setq uns-list (car ret-val))
    (setq un-list (cdr ret-val))
    (should uns-list)
    (setq something (shu-match-merge-namespaces-with-class-list class-list uns-list log-buf top-name))
    (should something)
    (setq proc-classes (car something))
    (setq proc-rlists (cdr something))
    (setq something (shu-match-add-names-to-class-list un-list proc-classes proc-rlists log-buf top-name))
    (setq proc-classes (car something))
    (setq proc-rlists (cdr something))
    (setq proc-classes (remove-class-duplicates proc-classes log-buf))
    (cons token-list proc-rlists)
    ))


;;; shu-cpp-mch-funs.el ends here
