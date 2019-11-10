;;; shu-match.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-match
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

;; Functions that use functions in shu-cpp-match.el


;;; Code:

(provide 'shu-match)
(require 'shu-cpp-match)




;;
;;  shu-match-find-variables
;;
(defun shu-match-find-variables ()
  "Find what might be all of the variable declarations in a header file by doing
a reverse tokenized scan looking for all occurrences of operator \";\" followed
by something that matches the regular expression for a C++ name.  Then take each
line that matches and put it in the buffer \"**shu-vars**\"."
  (interactive)
  (let ((token-list)
        (ret-val))
    (save-excursion
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
      (setq ret-val (shu-match-find-semi-names token-list)))
    ))




;;
;;  shu-match-find-semi-names
;;
(defun shu-match-find-semi-names (token-list)
  "With a match list that is a semi-colon followed by the regular expression for
a C++ name, do a reverse tokenized match for all occurrences, then take each line
that holds a match and put it into the buffer \"**shu-vars**\","
  (let ((find-name-list
         (list  ;; semicolon followed by name
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    ";")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (tlist token-list)
        (rlist)
        (ret-val)
        (pret-val)
        (something t)
        (gb (get-buffer-create "**shu-vars**"))
        (token-info)
        (spoint)
        (bol)
        (eol)
        (line-no)
        (line)
        (count 0)
        (pcount))
    (setq ret-val (shu-cpp-all-search-match-tokens rlist find-name-list tlist))
    (setq tlist (car ret-val))
    (setq rlist (cdr ret-val))
    (while rlist
      (setq token-info (car rlist))
      (setq spoint (shu-cpp-token-extract-spoint token-info))
      (goto-char spoint)
      (setq bol (line-beginning-position))
      (setq eol (line-end-position))
      (setq line-no (shu-format-num (line-number-at-pos) 5))
      (setq line (buffer-substring-no-properties bol eol))
      (princ (concat line-no ". " line "\n") gb)
      (setq count (1+ count))
      (setq rlist (cdr rlist)))
    (if (= count 0)
        (message "%s" "No names found.")
      (setq pcount (shu-group-number count))
      (message "%s lines added to %s" pcount (buffer-name gb)))
    ))




(defconst shu-cpp-match-colon-name
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
  "A repeating side list to match zero or more instances of {:: <name>}")




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
                                                  shu-cpp-match-colon-name)
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                               ";")
    )
   )
  )


;;
;;  shu-cpp-match-namespace-list
;;
(defconst shu-cpp-match-namespace-list
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
                                  shu-cpp-match-namespace-forms)
    )
   )
  )



;;
;;  shu-cpp-match-namespace-list-single
;;
(defconst shu-cpp-match-namespace-list-single
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
                                 shu-cpp-match-namespace-forms)
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
(defconst shu-cpp-match-using-forms
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
                                                  shu-cpp-match-colon-name)
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
;;  shu-cpp-match-many-using-list
;;
(defconst shu-cpp-match-many-using-list
  (list
   (list  ;; "namespace <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                  shu-cpp-match-namespace-forms)
    )

   (list  ;; "using namespace <name>;"
    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                  shu-cpp-match-using-forms)
    )

   )
  "These two lists match what may follow the key word \"using\".  The first list
matches the key word \"namespace\" followed by any of the different name types
that may follow \"using namespace\".  The second list matches any of the name
forms that may following the key word \"using\" when it is not followed by the
key word \"namespace\"." )



;;
;;  shu-cpp-match-using-list-single
;;
(defconst shu-cpp-match-using-list-single
  (list  ;; "using namespace <name>;"
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             t shu-cpp-token-type-kw
                             "using")
   (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                 shu-cpp-match-many-using-list)
   )
  "This is a single list that is the top level list for matching anything
that may follow the key word \"using\".")





;;
;;  shu-match-find-all-using-internal
;;
(defun shu-match-find-all-using-internal (token-list)
  "Given a token list, return two different lists.  The first is a list of all
\"using namespace\" statements.  The second is a list of all \"using\"
statements that are not \"using namespace\" statements.  \"using namespace
std;\" is an example of the first type.  \"using std::string\" is an example of
the second type.
The return value of this function is a single cons cell in which the cdr points
to the first list and the car points to the second list.
If neither list is present, then the return value is nil."
  (let ((ret-val t)
        (tlist token-list)
        (rlist)
        (nn)
        (token-info)
        (token-type)
        (token)
        (uns-list)
        (un-list))
    (while ret-val
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
      (when ret-val
        (setq tlist (car ret-val))
        (setq rlist (cdr ret-val))
        (setq rlist (nreverse rlist))
        (setq nn (cdr rlist))
        (setq token-info (car nn))
        (setq token-type (shu-cpp-token-extract-type token-info))
        (setq token (shu-cpp-token-extract-token token-info))
        (if (and
             (eq token-type shu-cpp-token-type-kw)
             (string= token "namespace"))
            (push rlist uns-list)
          (push rlist un-list))
        (setq rlist nil)))
    (when uns-list
      (setq uns-list (nreverse uns-list)))
    (when un-list
      (setq un-list (nreverse un-list)))
    (when (or uns-list un-list)
      (setq ret-val (cons uns-list un-list)))
    ret-val
    ))



;;
;;  shu-match-find-any-using-internal
;;
(defun shu-match-find-any-using-internal (token-list)
  "Given a token list, return a list of all \"using namespace\" statements and
all \"using\" statements that are not \"using namespace\" statements.  \"using
namespace std;\" is an example of the first type.  \"using std::string\" is an
example of the second type.  This is used to find out whether or not a file of
code contains any such statements and to identify them."
  (let ((ret-val t)
        (tlist token-list)
        (rlist)
        (rlists))
    (while ret-val
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
      (when ret-val
        (setq tlist (car ret-val))
        (setq rlist (cdr ret-val))
        (setq rlist (nreverse rlist))
        (push rlist rlists)
        (setq rlist nil)))
    (setq rlists (nreverse rlists))
    rlists
    ))



;;
;;  shu-match-using-namespace-string
;;
(defun shu-match-using-namespace-string (rlist &optional top-name)
  "Given an RLIST that contains a \"using namespace\" statement, return the string
that is the fully qualified namespace name.  If the first part of the name is the
optional TOP-NAME, it is omitted from the final result."
  (let ((nlist (cddr rlist))
        (sep-char "")
        (name "")
        (x 0)
        (token-info)
        (token-type)
        (token))
    (while nlist
      (setq token-info (car nlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq token (shu-cpp-token-extract-token token-info))
      (if (and
           (eq x 0)
           (eq token-type shu-cpp-token-type-uq)
           top-name
           (stringp top-name)
           (string= token top-name))
          nil
        (when (eq token-type shu-cpp-token-type-uq)
          (setq name (concat name sep-char token))
          (setq sep-char "::")))
      (setq x (1+ x))
      (setq nlist (cdr nlist)))
    name
    ))




;;
;;  shu-match-using-string
;;
(defun shu-match-using-string (rlist &optional top-name)
  "Given an RLIST that contains a \"using\" statement (as opposed to \"using
namespace\"), return two strings.  One is the class name.  The other is the
fully qualified namespace name.  For example, if the statement is \"using
std::string,\" the fully qualified namespace name is \"std\" and the class name
is \"string\".
The two strings are returned in a cons cell whose car is the namespace name and
whose cdr is the class name."
  (let ((nlist (cdr rlist))
        (rlist)
        (sep-char "")
        (name "")
        (x 0)
        (count 0)
        (cx 0)
        (token-info)
        (token-type)
        (token)
        (ptoken)
        (class-name))
    (setq rlist nlist)
    (setq count 0)
    ;;; Find the class name on the end
    (while rlist
      (setq token-info (car rlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq token (shu-cpp-token-extract-token token-info))
      (when (and
           (eq token-type shu-cpp-token-type-op)
           (string= token ";"))
        (setq cx (1- count))
        (setq class-name ptoken))
      (setq ptoken token)
      (setq count (1+ count))
      (setq rlist (cdr rlist)))
    ;;; Find the namespace name
    (setq count 0)
    (setq rlist nlist)
    (while (< count cx)
      (setq token-info (car rlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq token (shu-cpp-token-extract-token token-info))
      (if (and
           (eq x 0)
           (eq token-type shu-cpp-token-type-uq)
           top-name
           (stringp top-name)
           (string= token top-name))
          nil
        (when (eq token-type shu-cpp-token-type-uq)
          (setq name (concat name sep-char token))
          (setq sep-char "::")))
      (setq x (1+ x))
      (setq rlist (cdr rlist))
      (setq count (1+ count))
      (setq x (1+ x)))
    (cons name class-name)
    ))




;;
;;  shu-match-get-start-end-pos
;;
(defun shu-match-get-start-end-pos (rlist &optional whole-lines)
  "Given an RLIST return the beginning and end positions.  The beginning position is
the position of the first character of the first token.  The end position is the
position of the last character of the last token.  These two are returned as a cons
cell whose car is the beginning position and whose cdr is the end position.  If
the optional WHOLE-LINES is true, the start position is that of the beginning of
the line on which the start falls and the end position is that of the end of
the line on which the end falls."
  (let ((token-info (car rlist))
        (ptoken-info)
        (spoint)
        (epoint))
    (setq spoint (shu-cpp-token-extract-spoint token-info))
    (while rlist
      (setq token-info (car rlist))
      (setq rlist (cdr rlist))
      (when (not rlist)
        (setq epoint (shu-cpp-token-extract-epoint token-info))))
    (when whole-lines
      (save-excursion
        (goto-char spoint)
        (setq spoint (line-beginning-position))
        (goto-char epoint)
        (setq epoint (line-end-position))))
    (cons spoint epoint)
    ))





;;
;;  shu-cpp-rmv-using
;;
(defun shu-cpp-rmv-using (class-list &optional top-name)
  "Remove \"using namespace\" directives from a C++ file, adding the appropriate
namespace qualifier to all of the unqualified class names.  CLASS-LIST is an
a-list in which the car of each entry is a namespace and the cdr of each entry
is a list of class names.  Here is an example of such an a-list:

     (list
      (cons \"std\"    (list \"set\" \"string\" \"vector\"))
      (cons \"world\"  (list \"Hello\" \"Goodbye\")))

TOP-NAME, if present is a higher level namespace.  Given a top level namespace
of \"WhammoCorp\", then the following line:

     using namespace WhammoCorp::world;

would be interpreted as though it had been written:

     using namespace world;"
  (let ((log-buf (get-buffer-create "**shu-chgs**")))
    (shu-match-internal-rmv-using class-list log-buf top-name)
    ))


;;
;;  shu-match-internal-rmv-using
;;
(defun shu-match-internal-rmv-using (class-list log-buf &optional top-name)
  "This is an overview of the entire process used to remove both the using
namespace statements (e.g., \"using namespace std;\") and the using statements
(e.g., \"using std::string;\").

The input is a class list which is an alist in which the key for each entry is a
namespace name and the value for each entry is a list of class names that are
qualified by the namespace name.  This is an example of a class list:

     (list (cons \"std\" (list \"set\" \"string\" \"vector\")) (cons \"world\"
      (list \"Hello\" \"Goodbye\")))

The first step is to tokenize the entire buffer and then use match functions to
find each instance of \"using namespace\" and each occurrence of \"using name\".
Each of these statements is represented by a list of token-info.

If any \"using namespace\" statements are found, they are merged with the class
list to form a new class list that contains only those namespaces for which a
\"using namespace\" statement was found in the buffer.

A separate list is kept of the \"using namespace\" statements that have no
corresponding entries in the class list.  This is printed out later as a
diagnostic message.

The next step is to merge in the namespaces from the \"using name\" statements.
Each \"using name\" statement contributes one namespace name and one class name.

The original class list might have contained duplicate class name instances
within a given namespace.  The merging in the of the \"using name\" statements
may also have created duplicate class names within a namespace, so the next
thing we do is remove any duplicate class names within a given namespace.  For
example, if the input class list contained \"std . string set map\" and the
buffer contains both \"using namespace std;\" and \"using std::string;\", the
newly merged class list will contain the class name \"string\" twice under the
namespace \"std\".

Once we have the updated class list with duplicates removed, we create two new
data structures.  One is a hash table in which the key is a class name and the
value is the name of the namespace that qualifies that class name.  This will be
used to determine if an unquoted token is an unqualified class name.  The other
is an alist in which the key is a class name and the value is zero.  This will
be used to count the replacement count for each class.

In creating the hash table, we may discover that one class name maps to more
than one namespace name.  If that happens there is an unresolvable ambiguity and
the operation must cease.

The next step is to remove from the token list (from the tokenized buffer), the
lists of token-info that represent the \"using namespace\" and \"using name\"
statements that we are processing.  This prevents any subsequent scan from
seeing them again.

Next, we erase the \"using namespace\" statements and \"using name\" statements
from the buffer.  We do this by replacing the statements with an equivalent
amount of whitespace, which preserves the positions of all of the other tokens
in the buffer.

Some of the class names in the class list may also appear in #include statements
in the buffer.  For example, if we are trying to remove \"using namespace
std;\", the buffer may well contain #include <set> or #include <map>.  These
instances of set and map are file names.  They are not class names that should
have the \"std\" qualifier added to them.

To handle this case we build another hash table.  This one contains class names
as its key.  It is the intersection of class names and names found in include
statements.  The value of each entry in the hash table is the list of spoints
that are the start point for each name within its include statement.  It is a
list of spoints because the same name may appear in multiple #include
statements.

Then we go through the token list.  Whenever we encounter an unquoted token, we
look it up in the hash table to see if this is a class name that we should
qualify.  If the token exists in the hash table, we then look at its context to
see if it really looks like a class name.

IF the putative class name is preceded by any of \"::\", \".\", or \"->\", then
we assume that it is either a qualified class name or a function name.  There
are other various checks that can be found in the function
shu-match-find-unqualified-class-names.  One of the checks is to see if it
matches an instance contained in an #include statement, which is done with the
hash of include names described above.

Each time we find an unqualified class name, we push its token-info onto a new
list of class names that need to be qualified.  Note that we use push so the
list is backwards with respect to buffer order.  The first item in the list is
that last unqualified class name in the buffer.

This means that we can use the list directly to add the namespace qualifier to
each unqualified class name.  Since we are going through the buffer backwards,
adding a qualifier to an unqualified class name does not change the position of
any other unqualified class names in the buffer.

While adding namespace qualifiers to all of the unqualified class names, we also
accumulate a change count for each class name.

When all of the unqualified class names have been qualified, we display the
final change counts in a buffer and emit a message with the sum of all the
change counts."
  (let ((token-list)
        (ret-val)
        (uns-list)
        (un-list)
        (something)
        (proc-val)
        (np-rlists)
        (proc-classes)
        (proc-rlists)
        (class-ht)
        (incl-ht)
        (count-alist)
        (ret-val)
        (symbol-count)
        (clist))
    (save-excursion
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (setq ret-val (shu-match-find-all-using-internal token-list))
      (if (not ret-val)
          (progn
            (message "%s" "No using namespace directives found"))
        (setq uns-list (car ret-val))
        (setq un-list (cdr ret-val))
        (when uns-list
          (setq something (shu-match-merge-namespaces-with-class-list class-list uns-list log-buf top-name))
          (when something
            (setq np-rlists (cdr something))
            (setq proc-val (car something))
            (setq proc-classes (car proc-val))
            (setq proc-rlists (cdr proc-val))))
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
          (setq incl-ht (shu-match-fetch-include-hash-table token-list class-ht))
          (setq ret-val (shu-match-find-unqualified-class-names class-ht incl-ht token-list proc-classes log-buf))
          (setq symbol-count (car ret-val))
          (setq clist (cdr ret-val))
          (if clist
              (progn
                (setq count-alist (shu-match-make-count-alist-from-hash class-ht))
                (shu-match-qualify-class-names class-ht count-alist clist np-rlists (length token-list) symbol-count log-buf))
            (message "%s" "No unqualified class names found")))))
    ))



;;
;;  shu-match-find-unqualified-class-names
;;
(defun shu-match-find-unqualified-class-names (class-ht incl-ht token-list proc-classes log-buf)
  "Go through all of the tokens looking at any unquoted token that is in the
hash table of unqualified class names.  When an instance of a class name is
found, check to see it it is preceded by \"::\", \".\", or \"->\".  \"::\"
indicates that it is probably qualified.  \".\" or \"->\" indicate that it is
probably a function name.

Check also to see if it is followed by \")\" or \"[\", which probably indicates
that it is a variable name.

Next, check to see if it is wrapped in an #include statement.

If it survives all of those checks, it is probably an unqualified class name, in
which case its token-info is pushed onto a list.  The list is one of the values
returned from this function.  Since each token-info is pushed onto the list, the
list is returned in reverse order.  i.e., the last token in the file is the
first in the list.

At this point it is possible to visit each token in the list, which is in
reverse order in the file, look up each token, and insert in front of it its
qualifying namespace.

The other return value from this function is the count of the number of unquoted
tokens that were looked up in the hash table of all of the class names,
CLASS-HT..

The actual return value from this function is a cons cell whose car is the
symbol search count and whose cdr is the list of tokens that represent
unqualified class names to be qualified."
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
        (symbol-count 0)
        (clist))
    (while tlist
      (setq blocked nil)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq symbol-count (1+ symbol-count))
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
              (setq next-token (shu-cpp-token-extract-token next-token-info))
              (setq next-token-type (shu-cpp-token-extract-type next-token-info))
              (if
                  (and
                   (= next-token-type shu-cpp-token-type-op)
                   (or
                    (string= next-token "(")
                    (string= next-token "[")
                    ))
                  (setq blocked t)
                (when (shu-match-rmv-might-be-include incl-ht token token-info)
                  (setq blocked t)))))
          (when (not blocked)
            (push token-info clist))))
      (setq last-token token)
      (setq last-token-type token-type)
      (setq tlist (shu-cpp-token-next-non-comment tlist)))
    (cons symbol-count clist)
    ))


;;
;;  shu-match-qualify-class-names
;;
(defun shu-match-qualify-class-names (class-ht count-alist clist np-rlists token-count symbol-count log-buf)
  "CLASS-HT is the hash table that maps a class name to its containing namespace
name.  COUNT-ALIST is the alist that counts the number of times each class
name has been qualified by its enclosing namespace.  CLIST is the list of
token-info, each of which represents an unqualified class name.  The list is in
reverse order, which is important.  It means that one can add a qualification to
one class name in the list without changing the location of any other class
names, which are above the current one in the buffer.  NP-RLISTS is a list of
rlists, each of which represents a \"using namespace\" statement for which
there is no corresponding entry in the class list.  There are the \"using
namespace\" statements that we will not be processing..

This function goes to the position of each unqualified class name, finds its
containing namespace in the hash table, and inserts the containing namespace
followed by \"::\" in front of the unqualified class name.

After it inserts the qualifying namespace, it increments in COUNT-ALIST the number
of times that the class name was explicitly qualified."
  (let ((nprl np-rlists)
        (rlist)
        (ret-val)
        (spoint)
        (epoint)
        (ns-code)
        (lnum)
        (plnum)
        (ns-line)
        (ns-lines)
        (token-info)
        (token)
        (spoint)
        (hv))
    (while nprl
      (setq rlist (car nprl))
      (setq ret-val (shu-match-get-start-end-pos rlist t))
      (setq spoint (car ret-val))
      (setq epoint (cdr ret-val))
      (setq ns-code (buffer-substring-no-properties spoint epoint))
      (setq lnum (line-number-at-pos spoint))
      (setq plnum (shu-format-num lnum 6))
      (setq ns-line (concat plnum ". " ns-code "\n"))
      (push ns-line ns-lines)
      (setq nprl (cdr nprl)))
    (setq ns-lines (nreverse ns-lines))
    (while clist
      (setq token-info (car clist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq spoint (shu-cpp-token-extract-spoint token-info))
      (goto-char spoint)
      (setq hv (gethash token class-ht "????"))
      (insert (concat hv "::"))
      (shu-match-increment-class-count count-alist token)
      (setq clist (cdr clist)))
    (shu-match-rmv-show-class-count count-alist class-ht np-rlists ns-lines token-count symbol-count log-buf)
    ))




;;
;;  shu-match-rmv-might-be-include
;;
(defun shu-match-rmv-might-be-include (incl-ht token token-info)
  "INCL-HT is a hash table whose key is a name that was found in an include
statement and whose value is a list of all of the spoints of all of the
occurrences of the name in include statements.  It is a list because the same
name may be included multiple times.

If the current token matches one of the names in INCL-HT and the spoint of the
token is a member of the list of spoints in the entry in INCL-HT, then the
current name is enclosed in an include statement and should not have a namespace
qualifier added to it.

This function returns true if the name is wrapped in an include statement."
  (let ((spoint (shu-cpp-token-extract-spoint token-info))
        (blocked)
        (spoint-list))
    (when incl-ht
      (setq spoint-list (gethash token incl-ht))
      (when spoint-list
        (when (member spoint spoint-list)
          (setq blocked t))))
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
  (let ((pc proc-classes)
        (ce)
        (cl))
    (while pc
      (setq ce (car pc))
      (setq cl (cdr ce))
      (setq cl (delete-dups cl))
      (setcdr ce cl)
      (setq pc (cdr pc)))
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
modified PROC-CLASSES and the cdr is the modified PROC-RLISTS."
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
        (push class-name cl)
        (setcdr ce cl))
      (setq un-list (cdr un-list)))
    (cons proc-classes proc-rlists)
    ))



;;
;;  shu-match-merge-namespaces-with-class-list
;;
;;
;;  This function returns three values:
;;
;;  1. np-rlists is a list of rlists, each of which holds a "using namespace"
;;     statement for which no corresponging entry exsts in the class list.
;;     These are the "using namespace" statements that we will not process.
;;
;;  2. proc-rlists is a list of rlists, each of which holds a "using namespace"
;;     that has a corresponding entry in the class list.  These are the "using
;;     namespace" statements that we will process.
;;
;;  3. proc-classes is the alist of all of the namespaces and classes that we
;;     will process, with the namespace name being the key and a list of class
;;     names within the namespace name as the value.
;;
;;  The structure of the return value is as follows:
;;
;;
;;  ret-val (Single returned cons cell):
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> np-rlists
;;        |
;;        +-------------> proc-val
;;
;;
;;
;;  proc-val:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> proc-rlists
;;        |
;;        +-------------> proc-classes
;;
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
  (let ((rlist)
        (ns-name)
        (ce)
        (proc-classes)
        (proc-rlists)
        (np-rlists)
        (proc-val)
        (ret-val)
        (spoint)
        (epoint))
    (while uns-list
      (setq rlist (car uns-list))
      (setq ns-name (shu-match-using-namespace-string rlist top-name))
      (setq ce (assoc ns-name class-list))
      (if (not ce)
          (push rlist np-rlists)
        (when (not (assoc ns-name proc-classes))
          (push ce proc-classes)
          (push rlist proc-rlists)))
      (setq uns-list (cdr uns-list)))
    (when np-rlists
      (setq np-rlists (nreverse np-rlists)))
    (when proc-classes
      (setq proc-classes (nreverse proc-classes)))
    (when proc-rlists
      (setq proc-rlists (nreverse proc-rlists)))
    (setq proc-val (cons proc-classes proc-rlists))
    (setq ret-val (cons proc-val np-rlists))
    (when (not (and proc-classes proc-rlists))
      (setq ret-val nil))
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
(defun shu-match-rmv-show-class-count (count-alist class-ht np-rlists ns-lines token-count symbol-count log-buf)
  "Put into the log buffer the count of class names that were qualified."
  (let (
        (bfn (buffer-file-name))
        (cp)
        (class-name)
        (ns-name)
        (count)
        (full-name)
        (clist)
        (sum 0)
        (pcount)
        (psum)
        (np-stmt "")
        (ns-line)
          )
    (when bfn
      (princ (concat "\nStart rmv-using for file: " bfn "\n\n") log-buf)
      )
    (when np-rlists
      (setq np-stmt (format "%d namespaces not in class list. " (length np-rlists)))
      (princ "The following using namespace statemnts have no corresponding class list entry:\n" log-buf)
      (while ns-lines
        (setq ns-line (car ns-lines))
        (princ ns-line log-buf)
        (setq ns-lines (cdr ns-lines))
        )
      (princ "\n" log-buf)
      )
    (princ (format "%s tokens generated, %s symbols scanned.\n"
                   (shu-fixed-format-num token-count 0) (shu-fixed-format-num symbol-count 0)) log-buf)
    (princ "Count of qualified classes:\n" log-buf)
    (while count-alist
      (setq cp (car count-alist))
      (setq class-name (car cp))
      (setq count (cdr cp))
      (setq ns-name (gethash class-name class-ht))
      (setq full-name (concat ns-name "::" class-name))
      (push (cons full-name count) clist)
      (setq count-alist (cdr count-alist))
      )
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
      (setq clist (cdr clist))
      )
    (setq psum (shu-fixed-format-num sum 0))
    (if (and log-buf (bufferp log-buf))
        (message "%s%s class names qualified.  See buffer %s" np-stmt psum (buffer-name log-buf))
      (message "%s%s class names qualified." np-stmt psum )
        )
    ))




;;
;;  shu-match-fetch-include-hash-table
;;
(defun shu-match-fetch-include-hash-table (token-list class-ht)
  "TOKEN-LIST is the list of tokenized text from the buffer.  CLASS-HT is the
hash table that holds as its keys the names of all of the classes that we will
process.

This function finds all of the names in #include statements whose included file
name matches a class name in CLASS-HT.  It then builds and returns a new hash
table in which the key is the name of an included file whose name matches a
class name and whose value is a list of the spoints of the included file names.

It is a list of spoints, as opposed to a single point, because an include
statement with a given file name may appear several times.

For example, in the following code:

      #include <set>
      #include <map>
      #
       include
            <set>

      using namespace std;

      set     x;
      map     y;

the member variables x and y should be changed to std::set and std::map
respectively, but none of the occurrences of set or map in the include
statements should be changed."
  (let ((incl-list)
        (incl-ht))
    (setq incl-list (shu-match-find-all-some-include token-list))
    (when incl-list
      (setq incl-ht (shu-match-make-include-hash-table incl-list class-ht)))
    incl-ht
    ))




;;
;;  shu-cpp-match-some-include
;;
(defconst shu-cpp-match-some-include
   (list  ;; #include < name >
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                              "#")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-uq
                              "include")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                              "<")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                              ">"))
  "A list of match-info that matches \"#include <name>\".")






;;
;;  shu-match-find-all-some-include
;;
(defun shu-match-find-all-some-include (token-list)
  "Given a token list, return a list of tokens that represent all of the simple
names found in #include < name >, where \"name\" is a C++ name.  This search
will neither find nor return a name with a . in it.  This is a limited search
designed to find names that might be mistaken for class names to be qualified.

For example, if we are removing the namespace \"std\", then one of the names we
may wish to qualify is \"set\".  Buf if we encounter \"#include <set>\", we do
not want to transform that into \"#include <std::set>\".

Names of include files delimited by quotes will not be seen in the scan because
those are inside strings.  So we only want to find names in include statements
that are delimited by angle brackets and do not include a . in them."
  (let ((ret-val t)
        (tlist token-list)
        (rlist)
        (token-info)
        (incl-list))
    (while ret-val
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-some-include tlist))
      (when ret-val
        (setq tlist (car ret-val))
        (setq rlist (cdr ret-val))
        (setq token-info (car rlist))
        (push token-info incl-list)
        (setq rlist nil)))
    incl-list
    ))




;;
;;  shu-match-make-include-hash-table
;;
(defun shu-match-make-include-hash-table (incl-list class-ht)
  "INCL-LIST is the list of token-info, each of which represents a name found in
an include statement.  CLASS-HT is the hash table whose keys are the names of
the classes that we will be searching for.

For each name that appears in both INCL-LIST and CLASS-HT, create a new entry in
a new hash table whose key is the name of the class that has been found in one
or more include statements and whose value is a list of the spoints in which the
class name has been found in one or more include statements.

Whenever we find something that appears to be an unqualified class name, we can
look in this hash table to see if this occurrence of the name is one that
appears in an include statement and avoid adding a namespace qualifier if that
is the case."
  (let ((incl-alist)
        (incl-ht)
        (token-info)
        (token)
        (spoint)
        (hv)
        (ae)
        (al)
        (spoint-list))
    (while incl-list
      (setq token-info (car incl-list))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq hv (gethash token class-ht))
      (when hv
        (setq spoint (shu-cpp-token-extract-spoint token-info))
        (setq ae (assoc token incl-alist))
        (if ae
            (progn
              (setq al (cdr ae))
              (push spoint al)
              (setcdr ae al))
          (setq ae (cons token (list spoint)))
          (push ae incl-alist)))
      (setq incl-list (cdr incl-list)))
    (when incl-alist
      (setq incl-ht (make-hash-table :test 'equal :size (length incl-alist)))
      (while incl-alist
        (setq ae (car incl-alist))
        (setq token (car ae))
        (setq spoint-list (cdr ae))
        (puthash token spoint-list incl-ht)
        (setq incl-alist (cdr incl-alist))))
    incl-ht
    ))



;;
;;  shu-match-set-alias
;;
(defun shu-match-set-alias ()
  "Set the common alias names for the functions in shu-match,
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'find-all-variables 'shu-match-find-variables)
  )


;;; shu-match.el ends here
