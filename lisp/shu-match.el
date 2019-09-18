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
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
    (setq ret-val (shu-match-find-semi-names token-list))
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
        (line))
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
      (setq rlist (cdr rlist)))
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
  "A repeating side list to match zero or more intances of {:: <name>}")




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
  "This is a sigle list that is the top level list for matching anything
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
;;  shu-match-set-alias
;;
(defun shu-match-set-alias ()
  "Set the common alias names for the functions in shu-match,
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'find-all-variables 'shu-match-find-variables)
  )


;;; shu-match.el ends here
