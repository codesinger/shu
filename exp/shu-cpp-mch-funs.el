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
        )
    (princ "class-list: " log-buf)(princ class-list log-buf)(princ "\n" log-buf)
    (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (if (not ret-val)
        (progn
          (message "%s" "No using namespace directives found")
          )
      (setq uns-list (car ret-val))
      (setq un-list (cdr ret-val))
      (when uns-list
        (setq something (process-uns-list class-list uns-list log-buf top-name))
        (if (not something)
            (message "%s" "Failure")
          (setq proc-classes (car something))
          (setq proc-rlists (cdr something))
          (setq something (add-ns-rlists un-list proc-classes proc-rlists log-buf top-name))
          (setq proc-classes (car something))
          (setq proc-rlists (cdr something))
          (princ "proc-classes9: " log-buf)(princ proc-classes log-buf)(princ "\n" log-buf)
          (princ "proc-rlists9: " log-buf)(princ proc-rlists log-buf)(princ "\n" log-buf)
          (setq proc-classes (remove-class-duplicates proc-classes log-buf))
          (princ "\n\nproc-rlists: " log-buf)(princ proc-rlists log-buf)(princ "\n" log-buf)
          (princ "\n\ntoken-list: " log-buf)(princ token-list log-buf)(princ "\n" log-buf)
          (setq ret-val (shu-match-remove-proc-rlists token-list proc-rlists log-buf))
          (setq token-list (car ret-val))
          (setq proc-rlists (cdr ret-val))
          (princ "\n\nproc-rlists2: " log-buf)(princ proc-rlists log-buf)(princ "\n" log-buf)
          (princ "\n\ntoken-list2: " log-buf)(princ token-list log-buf)(princ "\n" log-buf)
          )
        )

      )
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
  (let (
        (tlist token-list)
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
        (looking-for-end)
        )
    (setq sprl sorted-proc)
    (princ "\nsprl: " log-buf)(princ sprl log-buf)(princ "\n" log-buf)
    (while (and sprl tlist)
      (setq rlist (car sprl))
      (princ "\nrlist: " log-buf)(princ rlist log-buf)(princ "\n" log-buf)
      (setq ret-val (shu-match-get-start-end-pos rlist))
      (setq r-spoint (car ret-val))
      (setq r-epoint (cdr ret-val))
      (princ (format "r-spoint: %d, r-epoint: %d\n" r-spoint r-epoint) log-buf)
      (princ "lastp: " log-buf)(princ (car lastp) log-buf)(princ "\n" log-buf)
      (princ "tlist: " log-buf)(princ (car tlist) log-buf)(princ "\n" log-buf)
;;;      (setq lastp tlist)
      (setq first1 nil)
      (setq looking-for-start t)
      (while looking-for-start
        (setq token-info (car tlist))
        (setq spoint (shu-cpp-token-extract-spoint token-info))
        (if (= spoint r-spoint)
            (progn
              (setq looking-for-start nil)
              (when (equal tlist token-list)
                (setq first1 t)
                )
              )
          (setq lastp tlist)
          (setq first1 nil)
          (setq tlist (cdr tlist))
          )
        )
      ;; lastp is the item whose cdr will be changed
      ;; If first1 is true, head of list will be changed
      ;; tlist points to the start item (which might also be the end)
      (princ "\ntoken-info of start: " log-buf)(princ token-info log-buf)(princ "\n" log-buf)
      (princ "\nlastp: " log-buf)(princ (car lastp) log-buf)(princ "\n" log-buf)
      (princ "\ntoken-list: " log-buf)(princ token-list log-buf)(princ "\n" log-buf)
      (setq looking-for-end t)
      (while looking-for-end
        (setq token-info (car tlist))
        (setq epoint (shu-cpp-token-extract-epoint token-info))
        (if (= epoint r-epoint)
            (progn
              (setq looking-for-end nil)
              (princ "first1: " log-buf)(princ first1 log-buf)(princ "\n" log-buf)
              (princ "\ntoken-list: " log-buf)(princ token-list log-buf)(princ "\n" log-buf)
              (princ "\nlastp: " log-buf)(princ (car lastp) log-buf)(princ "\n" log-buf)
              (if first1
                  (setq token-list (cdr tlist))
                (setcdr lastp (cdr tlist))
                (setq tlist (cdr tlist))
                )
              )
          (setq tlist (cdr tlist))
          )
        )
      (princ "\ntoken-info of end: " log-buf)(princ token-info log-buf)(princ "\n" log-buf)
      (princ "\ntoken-list: " log-buf)(princ token-list log-buf)(princ "\n" log-buf)
      (setq sprl (cdr sprl))
      )
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
      (princ "cl1: " log-buf)(princ ce log-buf)(princ "\n" log-buf)
      (setq cl (cdr ce))
      (setq cl (delete-dups cl))
      (setcdr ce cl)
      (princ "cl2: " log-buf)(princ ce log-buf)(princ "\n" log-buf)
      (setq pc (cdr pc))
      )
    proc-classes
    ))



;;
;;  shu-test-remove-class-duplicates
;;
(ert-deftest shu-test-remove-class-duplicates ()
  (let (
        (gb (get-buffer-create "**goo**"))
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
         (cons "std "    (list
                           "string"
                           "string"
                           "set"
                           "set"
                           "map"
                           ))
         )
        )
       (proc-class)
        )
    (setq proc-class (remove-class-duplicates class-list gb))
    (princ "proc-class: " gb)(princ proc-class gb)(princ "\n" gb)
    ))


;;
;;  make-class-hash-table
;;
(defun make-class-hash-table (proc-classes log-buf)
  "PROC-CLASSES is the alist of all of the namespaces and classes that we will
process, wth the naespace name being the kay and a list of class names under the
namespace name as the value.  This function builds a hash table that inverts the
alist.  Each entry in the hash table has a class name as the key with the name
of the enclosing namespace as the value.
If two class names map to the same enclosing namespace name, then there is an
unresolvable ambiguity that terminates the operation."
  (interactive)
  (let (
        (pc proc-classes)
        (ht)
        (count 0)
        (ce)
        (cl)
        (ns-name)
        (class-name)
        (hv)
        )
    (while pc
      (setq ce (car pc))
      (setq cl (cdr ce))
      (setq count (+ count (length cl)))
      (setq pc (cdr pc))
      )
    (princ (format "Will have %d entries\n" count) log-buf)
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
              )
          (puthash class-name ns-name ht)
            )
        (setq cl (cdr cl))
        )
      (setq pc (cdr pc))
      )
    (princ "ht: " log-buf)(princ ht log-buf)(princ "\n" log-buf)
    ht
    ))


;;
;;  shu-test-make-class-hash-table
;;
(ert-deftest shu-test-make-class-hash-table ()
  (let (
        (gb (get-buffer-create "**goo**"))
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
                           ))
         )
        )
       (ht)
       (hv)
       )
    (setq ht (make-class-hash-table class-list gb))
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
;;  add-ns-rlists
;;
(defun add-ns-rlists (un-list proc-classes proc-rlists log-buf &optional top-name)
  "UN-LIST is the list of rlists that represent the \"using name\" statements.
PROC-CLASSES is the class list we will be using to do the processing.
PROC-RLISTS is the set of rlists that represents the \"using namespace\"
statements.  This function adds the\"using name\" directives, if any, to both
PROC-CLASSES and PROC-RLISTS.  It returns a cons cell in which the car is the
modified PROC-CLASSES and the cdr is the modified P{ROC-RLISTS."
  (let (
        (rlist)
        (ret-val)
        (ns-name)
        (class-name)
        (ce)
        (x)
        (cl)
        )
    (while un-list
      (princ "un-list: " log-buf)(princ un-list log-buf)(princ "\n" log-buf)
      (setq rlist (car un-list))
      (princ "rlist-un: " log-buf)(princ rlist log-buf)(princ "\n" log-buf)
      (push rlist proc-rlists)
      (setq ret-val (shu-match-using-string rlist top-name))
      (setq ns-name (car ret-val))
      (setq class-name (cdr ret-val))
      (princ (format "Looking for ns: '%s'\n" ns-name) log-buf)
      (setq ce (assoc ns-name proc-classes))
      (if (not ce)
          (progn
            (setq x (cons ns-name (list class-name)))
            (push x proc-classes)
            (princ "Adding ns: " log-buf)(princ proc-classes log-buf)(princ "\n" log-buf)
            )
        (setq cl (cdr ce))
        (push class-name cl)
        (princ "Adding cl: " log-buf)(princ proc-classes log-buf)(princ "\n" log-buf)
            )
      (setq un-list (cdr un-list))
      )
    (cons proc-classes proc-rlists)
    ))



;;
;;  process-uns-list
;;
(defun process-uns-list (class-list uns-list log-buf &optional top-name)

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
    (princ "uns-list: " log-buf)(princ uns-list log-buf)(princ "\n" log-buf)
    (while uns-list
      (setq rlist (car uns-list))
      (princ "rlist: " log-buf)(princ rlist log-buf)(princ "\n" log-buf)
      (setq ns-name (shu-match-using-namespace-string rlist top-name))
      (princ (format "ns-name: '%s'\n" ns-name) log-buf)
      (setq ce (assoc ns-name class-list))
      (princ "ce: " log-buf)(princ ce log-buf)(princ "\n" log-buf)
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
      (princ "The following using namespaces have no corresponding class list entry:\n" log-buf)
      (setq np-rlists (nreverse np-rlists))
      (while np-rlists
        (setq rlist (car np-rlists))
        (setq ret-val (shu-match-get-start-end-pos rlist t))
        (setq spoint (car ret-val))
        (setq epoint (cdr ret-val))
        (setq ns-code (buffer-substring-no-properties spoint epoint))
        (princ (concat "\n" ns-code "\n") log-buf)
        (setq np-rlists (cdr np-rlists))
        )
      )
    (when proc-classes
      (setq proc-classes (nreverse proc-classes))
      (princ "proc-classes: " log-buf)(princ proc-classes log-buf)(princ "\n" log-buf)
      )
    (when proc-rlists
      (setq proc-rlists (nreverse proc-rlists))
      (princ "proc-rlists: " log-buf)(princ proc-rlists log-buf)(princ "\n" log-buf)
      )
    (if (and proc-classes proc-rlists)
        (setq ret-val (cons proc-classes proc-rlists))
      (setq ret-val nil)
      )
    ret-val
    ))



;;
;;  shu-test-something-or-other
;;
(ert-deftest shu-test-something-or-other ()
  (let (
        (log-buf (get-buffer-create "**goo**"))
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
         "    using abc::std::string;\n /* Hello */"
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



;;; shu-cpp-mch-funs.el ends here
