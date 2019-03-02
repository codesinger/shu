;;; shu-visible.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2013 Stewart L. Palmer
;;
;; Package: shu-visible
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is also not part of the shu elisp pckage.                                      ;
;; This is experimental code that is unlikely to become part of the
;; shu slisp package in its current form.
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


(defconst shu-java-visible-buffer "*Visible*"
  "Name debug buffer.")

;;  THINGS TO ADD ...
;;
;;  1. Have some way of having shu-find-visible-space go part way through
;;     a file and stop at a point where it can be restarted.  So if looking
;;     for something like the first pakage statement one could search
;;     for the first "package", ask if it was visible by having
;;     find visible space go through the end of the package statement.
;;     If the package statement isn't visible, then search for the next
;;     "package" and have find-visible-space resume its parse where it
;;     had left off up until the end of this package statement, etc.
;;
;;  2. re-search-forward-visible accepts a search string and searches forward
;;     for an occurrence of the string that is visible.  This is done by
;;     calling a re-search-visible function with one of the parameters
;;     being re-search-forward so that it is easy to implement forwards and
;;     backwards versions.
;;
;;  3. re-search-forwards-visible-or-in-comment as above but will accept
;;     an occurrence that is in a comment (not in a string).  Returns a cons
;;     cell whose car contains (point) and whose cdr contains t if the string
;;     was found inside a comment.  Use this to check to see if an import
;;     statement should be added to the file.  If the import statement has
;;     been commented out, don't add a new one just explain that it is there
;;     but commented out.
;;
;;     re-search-forwards-visible-or-in-comment should, in general,
;;     look for the "most" visible occurrence.  If it finds one in a
;;     comment, remember that but keep looking to see if there is one
;;     that is not in a comment.
;;
;;  The first cons cell on the visible list contains the (buffer-size)
;;  at the time the list was built and the point at which the scan
;;  should resume if the list is to be extended.  get-visible-list is
;;  given an optional stopping point for its scan.  It stops at the
;;  first logical stopping point after the given point.
;;
;;  The (buffer-size) is used as a debug check.  Each time the visible
;;  list is extended we check to make sure that the buffer size has not
;;  changed.  If it has we raise an error.
;;
;;  Be careful here.  The buffer size can actually change as long as
;;  the changes occur in the buffer *after* the last point included in
;;  the current visible list.  So we probably need a function to fetch
;;  the last point in a visible list.  If we are making a change
;;  before that point we still don't need to discard the visible
;;  list.  Only need to prune it back until it no longer includes the
;;  parts of the buffer that have changed.
;;
;;  visible-search backward first does the search backward and then
;;  calls get-visible-list to build a list that includes the found
;;  string.  This is the only visible list needed because all searches
;;  are backwards.  Any finds will be covered by the first visible-list
;;  built.
;;
;;  visible-search-forward sarches forward and then calls
;;  get-visible-list to build a list that includes the found string.  If
;;  the found string is not visible the search continues and the visible
;;  list is extended after each found string.
;;
;;  This approach has two advantages.
;;
;;  1. It saves execution time and, more importantly,
;;
;;  2. Parts of the buffer that don't need to be scanned can have in
;;     them mal-formed strings and comments
;;
;;
;;  Even if one intends to find every occurrence in the buffer, the
;;  list should be built incrementally because there might not be any
;;  occurrences.  Or there might be only one near the top.
;;
;;
;;
;;  The visible search functions come in two different categories.
;;  One set of functions behaves much like search-forward,
;;  search-backward, re-search-forward, etc.  One can write lisp such
;;  as (if (visible-search-forward ...)
;;  The other set of functions always retuens a cons cell.  The car of
;;  the cons cell contains nil if the search failed and contains the
;;  value of point if the search succeeded.  The cdr of the cons cell
;;  points to the visibility list used for the search.  So while one
;;  could write (if (cs-visible-search-forward ...) it would make no
;;  sense because cs-visible-search-forward always returns a cons
;;  cell.  If the search fails the car of the cons cell is nil.  But
;;  the (if is evaluated against the car, which is always non-nil.
;;
;;

;;
;;  visible-search-forward
;;
(defun visible-search-forward (string &optional bound vcons)
 "Search forward for a visible STRING in the buffer.
A visible string is one that is not contained within a C++ or Java
comment or within a string.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-max).
Uses search-forward internally and thus retains its side affects with
respect to match-beginning, match-end, and replace-match."
  (interactive "sSearch: ")
  (car (shu-visible-search-forward 'search-forward nil string bound vcons))
)

;;
;;  visible-re-search-forward
;;
(defun visible-re-search-forward (string &optional bound vcons)
 "Same as visible-search-forward but uses regular expression search."
  (interactive "sRE search: ")
  (car (shu-visible-search-forward 're-search-forward nil string bound vcons))
)

;;
;;  visible-search-backward
;;
(defun visible-search-backward (string &optional bound vcons)
 "Search backward for a visible STRING in the buffer.
A visible string is one that is not contained within a C++ or Java
comment or within a string.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-min).
Uses search-backward internally and thus retains its side affects with
respect to match-beginning, match-end, and replace-match."
  (interactive "sSearch: ")
  (car (shu-visible-search-backward 'search-backward nil string bound vcons))
)

;;
;;  visible-re-search-backward
;;
(defun visible-re-search-backward (string &optional bound vcons)
 "Same as visible-search-backward but uses regular expression search."
  (interactive "sRE search: ")
  (car (shu-visible-search-backward 're-search-backward nil string bound vcons))
)

;;
;;  visible-search-forward-comment
;;
(defun visible-search-forward-comment (string &optional bound vcons)
 "Search forward for a STRING that is either visible or in a comment.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-max).
Uses search-forward internally and thus retains its side affects with
respect to match-beginning, match-end, and replace-match."
  (interactive "sSearch: ")
  (car (shu-visible-search-forward 'search-forward t string bound vcons))
)

;;
;;  visible-re-search-forward-comment
;;
(defun visible-re-search-forward-comment (string &optional bound vcons)
 "Same as visible-search-forward-comment but uses regular expression search."
  (interactive "sRE search: ")
  (car (shu-visible-search-forward 're-search-forward t string bound vcons))
)

;;
;;  visible-search-backward-comment
;;
(defun visible-search-backward-comment (string &optional bound vcons)
 "Search backward for a STRING that is either visible or in a comment.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-min).
Uses search-backward internally and thus retains its side affects with
respect to match-beginning, match-end, and replace-match."
  (interactive "sSearch: ")
  (car (shu-visible-search-backward 'search-backward t string bound vcons))
)

;;
;;  visible-re-search-backward-comment
;;
(defun visible-re-search-backward-comment (string &optional bound vcons)
 "Same as visible-search-backward-comment but uses regular expression search."
  (interactive "sRE search: ")
  (car (shu-visible-search-backward 're-search-backward t string bound vcons))
)

;;
;;  cs-visible-search-forward
;;
(defun cs-visible-search-forward (string &optional bound vcons)
 "Search forward for a visible STRING in the buffer.  Return cons cell.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-max).
Uses search-forward internally and thus retains its side affects with
  respect to match-beginning, match-end, and replace-match.
Note that a cons cell is always returned.  The car of the cons cell is
nil if STRING was not found and contains the value of point if STRING
was found.  The cdr of the cons cell contains the visible list used for
the search."
  (interactive "sSearch: ")
  (shu-visible-search-forward 'search-forward nil string bound vcons)
)

;;
;;  cs-visible-re-search-forward
;;
(defun cs-visible-re-search-forward (string &optional bound vcons)
 "Same as visible-search-forward but uses regular expression search."
  (interactive "sRE search: ")
  (shu-visible-search-forward 're-search-forward nil string bound vcons)
)

;;
;;  cs-visible-search-backward
;;
(defun cs-visible-search-backward (string &optional bound vcons)
 "Search backward for a visible STRING in the buffer.  Return cons cell
Set point to the beginning of the occurrence found, and return cons cell.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-min).
Uses search-backward internally and thus retains its side affects with
  respect to match-beginning, match-end, and replace-match.
Note that a cons cell is always returned.  The car of the cons cell is
nil if STRING was not found and contains the value of point if STRING
was found.  The cdr of the cons cell contains the visible list used for
the search."
  (interactive "sSearch: ")
  (shu-visible-search-backward 'search-backward nil string bound vcons)
)

;;
;;  cs-visible-re-search-backward
;;
(defun cs-visible-re-search-backward (string &optional bound vcons)
 "Same as cs-visible-search-backward but uses regular expression search."
  (interactive "sRE search: ")
  (shu-visible-search-backward 're-search-backward nil string bound vcons)
)

;;
;;  cs-visible-search-forward-comment
;;
(defun cs-visible-search-forward-comment (string &optional bound vcons)
 "Search forward for a STRING that is either visible or in a comment.
Set point to the end of the occurrence found, and return cons cell.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-max).
Uses search-forward internally and thus retains its side affects with
  respect to match-beginning, match-end, and replace-match.
Note that a cons cell is always returned.  The car of the cons cell is
nil if STRING was not found and contains the value of point if STRING
was found.  The cdr of the cons cell contains the visible list used for
the search."
  (interactive "sSearch: ")
  (shu-visible-search-forward 'search-forward t string bound vcons)
)

;;
;;  cs-visible-re-search-forward-comment
;;
(defun cs-visible-re-search-forward-comment (string &optional bound vcons)
 "Same as cs-visible-search-forward-comment but uses regular expression search."
  (interactive "sRE search: ")
  (shu-visible-search-forward 're-search-forward t string bound vcons)
)

;;
;;  cs-visible-search-backward-comment
;;
(defun cs-visible-search-backward-comment (string &optional bound vcons)
 "Search backward for a STRING that is either visible or in a comment.
Set point to the beginning of the occurrence found, and return cons cell.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-min).
Uses search-backward internally and thus retains its side affects with
  respect to match-beginning, match-end, and replace-match.
Note that a cons cell is always returned.  The car of the cons cell is
nil if STRING was not found and contains the value of point if STRING
was found.  The cdr of the cons cell contains the visible list used for
the search."
  (interactive "sSearch: ")
  (shu-visible-search-backward 'search-backward t string bound vcons)
)

;;
;;  cs-visible-re-search-backward-comment
;;
(defun cs-visible-re-search-backward-comment (string &optional bound vcons)
 "Same as cs-visible-search-backward-comment but uses regular expression search."
  (interactive "sRE search: ")
  (shu-visible-search-backward 're-search-backward t string bound vcons)
)


;;
;;  shu-visible-search-forward
;;
(defun shu-visible-search-forward (sfunc in-cmt string &optional bound vcons)
 "Internal visible visible-search-forward.
SFUNC is the search operator (either search-forward or re-search-forward).
If IN-CMT is true then strings may be found inside a comment.  STRING
is the string to find.  BOUND optionally limits the search.  VCONS is
the optional head of a visible list."
 (let (
  (vc         vcons)
  (found-pt   nil)
  (limit      t)
  (fp         nil)
  (rfp        nil)
  (got-it     nil)
      )
  (save-excursion
  (while limit
    (setq fp (funcall sfunc string bound t))
    (setq rfp (match-beginning 0))
    (if (not fp)
      (setq limit nil)
      (setq rfp (match-beginning 0))
      (setq vc (shu-find-all-visible-space vc fp))
      (if (shu-is-visible vc rfp)
        (setq got-it t)
        (when in-cmt
          (when (visible-in-comment vc rfp)
            (setq got-it t))))
      (when got-it
        (setq found-pt fp)
        (setq limit nil)))
  ))
 (when found-pt
   (goto-char found-pt))
 (cons found-pt vc)
))

;;
;;  shu-visible-search-backward
;;
(defun shu-visible-search-backward (sfunc in-cmt string &optional bound vcons)
 "Internal visible visible-search-backward.
SFUNC is the search operator (either search-forward or re-search-forward).
If IN-CMT is true then strings may be found inside a comment.  STRING
is the string to find.  BOUND optionally limits the search.  VCONS is
the optional head of a visible list."
 (let (
  (pstart    (point))
  (vc         nil)
  (found-pt   nil)
  (limit      t)
  (fp         nil)
  (got-it     nil)
      )
  (save-excursion
  (setq vc (shu-find-all-visible-space vcons pstart))
  (while limit
    (setq fp (funcall sfunc string bound t))
    (if (not fp)
      (setq limit nil)
      (if (shu-is-visible vc fp)
        (setq got-it t)
        (when in-cmt
          (when (visible-in-comment vc fp)
            (setq got-it t))))
      (when got-it
        (setq found-pt fp)
        (setq limit nil)))
  ))
 (when found-pt
   (goto-char found-pt))
 (cons found-pt vc)
))

;;
;;  shu-find-all-visible-space
;;
(defun shu-find-all-visible-space (vcons &optional end-pt)
 (let (
  (vc   nil)
      )
  (save-excursion
   (save-restriction
    (widen)
    (goto-char (point-min))
    (setq vc (shu-find-visible-space vcons end-pt))))
  vc
))

;;
;;  shu-find-visible-space
;;
(defun shu-find-visible-space (vcons &optional end-pt)
  "Return list describing all visible space in a Java, C, or C++ program.
Returns a list of cons cells.  Each cons cell describes the start and end
of space within the file that is \"visible\" (i.e., not inside of either
a comment or a string).  This is used by shu-is-visible to determine if a
point found by a search is \"visible\".  As an example, to find an import
statement one could simply use re-search-forward to find it.  But that will
return import statements that might be buried in a string or a comment.
After finding the import statement one can call shu-is-visible to see if
the thing found is visible.  If not visible, the search should continue."
  (let (
;    (lbuf (get-buffer-create "*foo*"))
   (visible-list    nil)
   (comment-list    nil)
   (limit           t)
   (vstart          (point))
   (vend            -1)
   (string-or-comment-start (regexp-opt (list "'" "\"" "//" "/*") nil))
   (comment-start-a (regexp-quote "/*"))
   (comment-end-a   (regexp-quote "*/"))
   (comment-start-b (regexp-quote "//"))
   (double-quote    (regexp-quote "\""))
   (single-quote    (regexp-quote "'"))
   (string-start    (regexp-opt (list "'" "\"") nil))
   (back-slash      (regexp-quote "\\"))
   (string-term     nil)
   (spoint          nil)
   (line-col        nil)
   (end-point       nil)
   (debug-on-error  t)
       )
   (save-match-data
   ; If input cons cell is empty, make it to nil pointers
   (when (not vcons)
     (setq vcons (cons nil nil)))
   ; Make an empty visible list if there isn't one
   (setq visible-list (car vcons))
   (when (not visible-list)
     (setq visible-list (shu-set-next-visible visible-list vstart))
     (setcar vcons visible-list))
   ; Make an empty comment list if there isn't one
   (setq comment-list (cdr vcons))
   (when (not comment-list)
     (setq comment-list (shu-set-next-visible comment-list vstart))
     (setcdr vcons comment-list))
   ; Fetch our starting point from the visible list.  If we created
   ; the list then the starting point is (point).
   (setq vstart (shu-next-visible (car vcons)))
   ; Establish the end point, if any.
   (setq end-point end-pt)
   (when (not end-point)
     (setq end-point (buffer-size)))
   ; If we have already scanned past the starting point there
   ; is nothing to do
   (if (> vstart end-point)
     (setq limit nil)
     (goto-char vstart))
;(print end-point (get-buffer lbuf))
   (while limit
     (if (not (re-search-forward string-or-comment-start nil t))
       (progn
         (setq vcons
            (shu-add-to-xcons vcons 1 (cons vstart (point-max))))
         (setq vcons (shu-set-next-visible-xc vcons (point-max)))
         (setq limit nil)
       )
       (backward-char 1)
       (if (looking-at string-start)
         (progn
          (setq spoint (point))
          (setq string-term (buffer-substring (point) (1+ (point))))
          (setq vstart (shu-next-visible (car vcons)))
          (setq vend (1- (point)))
          (when (not (= vend 0))
            (setq vcons
              (shu-add-to-xcons vcons 1 (cons vstart vend))))
          (setq vstart (shu-end-of-string string-term))
          (setq vcons (shu-set-next-visible-xc vcons vstart))
          (when (> vend end-point)
            (setq limit nil))
          (when (not vstart)
            (setq limit nil)
            (setq vcons nil)
            (setq line-col (shu-line-and-column-at spoint))
            (message "String at line %d column %d never ends."
                     (car line-col) (cdr line-col))
            (princ
              (format "***error***String at line %d column %d (%d) never ends.\n"
                     (car line-col) (cdr line-col) spoint)
                     (get-buffer shu-java-visible-buffer))
            (ding)))
          (backward-char 1)
          (if (looking-at comment-start-a)
            (progn
              (setq spoint (point))
              (setq vstart (shu-next-visible (car vcons)))
              (setq vend (1- (point)))
              (when (not (= vend 0))
                (setq vcons (shu-add-to-xcons vcons 1 (cons vstart vend))))
              (forward-char 2)
              (setq vstart (re-search-forward comment-end-a nil t))
              (setq vcons (shu-add-to-xcons vcons 2 (cons spoint (1- vstart))))
              (setq vcons (shu-set-next-visible-xc vcons vstart))
              (when (> vend end-point)
                (setq limit nil))
              (when (not vstart)
                (setq limit nil)
                (setq vcons nil)
                (setq line-col (shu-line-and-column-at spoint))
                (message "Comment at line %d, column %d (%d) never ends."
                          (car line-col) (cdr line-col) spoint)
                (princ
                (format"***error***Comment at line %d column %d never ends.\n"
                         (car line-col) (cdr line-col) )
                         (get-buffer shu-java-visible-buffer))
                (ding)))
            (if (looking-at comment-start-b)
              (progn
                (setq spoint (point))
                (setq vstart (shu-next-visible (car vcons)))
                (setq vend (1- (point)))
                (when (not (= vend 0))
                  (setq vcons
                    (shu-add-to-xcons vcons 1 (cons vstart vend))))
                (end-of-line)
;                (setq vstart (1+ (point)))
                (setq vstart (point))
                (when (> vstart (buffer-size))
                  (setq vstart (buffer-size)))
                (setq vcons (shu-add-to-xcons vcons 2 (cons spoint (1- vstart))))
                (setq vcons (shu-set-next-visible-xc vcons vstart))
                (when (> vend end-point)
                  (setq limit nil))
                (goto-char (point)))
              (error "Huh?")))))
   )
   vcons
)))

;;
;;  shu-end-of-string
;;
(defun shu-end-of-string (string-term)
  "Return the point that terminates the current quoted string in the buffer.
The single argument STRING-TERM is a string containing the character that
started the string (single or double quote).  Return nil if the current
string is not terminated in the buffer."
  (let (
   (limit     t)
   (epoint    nil)
   (string-end    (regexp-quote string-term))
   (back-slash-or-quote (regexp-opt (list "\\" string-term) nil))
        )
  (forward-char 1)
  (while limit
    (setq epoint (re-search-forward back-slash-or-quote nil t))
    (if (not epoint)
      (setq limit nil)
      (backward-char 1)
      (if (looking-at string-end)
        (progn
          (forward-char 1)
          (setq limit nil))
        (forward-char 2)))
  )
  epoint
))

;;
;;  shu-line-and-column-at
;;
(defun shu-line-and-column-at (arg)
"Return the line number and column number of the point passed in as an argument."
  (let (
    (line-no    -1)
    (col-no     -1)
       )
  (save-excursion
    (save-restriction
    (widen)
      (goto-char arg)
      (beginning-of-line)
      (setq line-no (1+ (count-lines 1 (point))))
      (setq col-no (1+ (- arg (point))))))
  (cons line-no col-no)
))


;;
;;  shu-is-visible
;;
(defun shu-is-visible (vcons pt)
  (shu-is-something vcons pt 'car)
)

;;
;;  visible-in-comment
;;
(defun visible-in-comment (vcons pt)
  (shu-is-something vcons pt 'cdr)
)

;;
;;  shu-is-something
;;
(defun shu-is-something (vcons pt part)
 (let (
  (is-vis         nil)
  (visible-list   nil)
  (visible-range  nil)
  (limit          t)
      )
  (when (not vcons)
    (error "shu-is-something called with nil vcons"))
  (setq visible-list (funcall part vcons))
  (setq visible-list (cdr visible-list))
  (while limit
    (setq visible-range (car visible-list))
    (if (and (>= pt (car visible-range))
             (<= pt (cdr visible-range)))
      (progn
        (setq is-vis t)
        (setq limit nil))
      (when (> (car visible-range) pt)
        (setq limit nil)))
    (setq visible-list (cdr visible-list))
    (when (not visible-list)
      (setq limit nil))
  )
 is-vis
))

(defun shu-sample-visibles()
  "Parse a jar file full of Java source to extract all of the class names."
  (interactive)
  (let (
    (debug-on-error t)
    (shu-java-buffer (get-buffer-create shu-java-visible-buffer))
    (file-ss  "\\s-*\\([a-zA-Z0-9\\/\\.\\_\\$]*\\)\\/\\([a-zA-Z0-9\\_\\$]*\\)\\.java")
    (pkg-ss   shu-java-package-ss)
    (pkg-buf  (create-file-buffer shu-base-jar-file-name))
    (file-name nil)
    (root-name nil)
    (class-name nil)
    (pkg-name nil)
    (item nil)
    (lengths nil)
    (lcount -1)
    (vlen    0)
    (vl      nil)
    (vc      nil)
    (i       0)
    (pmsg nil)
    (props nil)
    (tlist   nil)
    (vsum    0)
    (vnum    0)
    (vavg    0)
    (vmed    0)
    (vlong   -1)
    (vshort  999999)
    (len     0)
    (tlines   0)
    (ix      0)
    (xpart   0.0)
       )
     (save-current-buffer
       (set-buffer pkg-buf)
       (insert-file-contents shu-base-jar-file-name t)
       (after-find-file)   ; Required to parse a jar file
       (while (re-search-forward file-ss (buffer-size) t)
         (setq i (1+ i))
         (save-excursion
           (setq root-name (match-string 1))
           (setq class-name (match-string 2))
              ; Get rid of mouse face properties set by jar file parseer
           (set-text-properties 0 (length root-name) nil root-name)
           (set-text-properties 0 (length class-name) nil class-name)
           (setq file-name (concat root-name "/" class-name ".java"))
           (archive-extract)
          (setq lcount (count-lines (point-min) (buffer-size)))
          (setq tlines (+ tlines lcount))
          (setq pkg-name (shu-get-package-from-buffer))
          (setq pmsg (format "File %s\n"
                                 file-name))
           (princ pmsg (get-buffer shu-java-buffer))
          (setq vc (shu-find-visible-space nil))
          (setq vl (car vc))
          (setq vlen 0)
          (when vl
            (setq vlen (length vl)))
          (when (< vlen vshort)
            (setq vshort vlen))
          (when (> vlen vlong)
            (setq vlong vlen))
          (setq vsum (+ vsum vlen))
          (setq lengths (cons vlen lengths))
          (setq pmsg (format "File %s (%d lines) length = %d.\n"
                                 file-name lcount vlen))
           (princ pmsg (get-buffer shu-java-buffer))
           (kill-current-buffer)
           (message "Count: %d" i)
         )
       )
       (setq vavg (/ vsum i))
       (setq lengths (sort lengths '<))
       (setq len (length lengths))
       (setq vmed (nth (/ len 2) lengths))
       (print lengths (get-buffer shu-java-buffer))
       (setq pmsg
        (format "\nFiles: %d, lines = %d, shortest = %d, longest = %d, avg = %d, median = %d\n"
                        i   tlines vshort vlong vavg vmed))
           (princ pmsg (get-buffer shu-java-buffer))
       (setq xpart 0.01)
       (while (< xpart 1.0)
         (setq ix (round (* len xpart)))
         (setq vmed (nth ix lengths))
         (setq pmsg (format "  %f - %d\n" xpart vmed))
         (princ pmsg (get-buffer shu-java-buffer))
         (setq xpart (+ xpart 0.01))
       )
     )
))

;;
;;  shu-add-to-tlist
;;
(defun shu-add-to-tlist (tlist item)
  "Add an item to the tail of a tlist.  car of 1st element points to last."
 (let (
  (new1    (cons item nil))       ; cons cell for new (or perhaps only) item
  (last1   nil)
      )
  (if (not tlist)                 ; List is currently empty
    (setq tlist (cons new1 new1)) ; car & cdr both point to last item on list
                                  ; List is not empty
    (setq last1 (car tlist))      ; car of list points to last one
    (setcdr last1 new1)           ; Last one points forward to new one
    (setcar tlist new1))          ; car of list points to new last one
  tlist
))

;;
;;  shu-add-to-xcons
;;
(defun shu-add-to-xcons (tcons tnum item)
  "Add to one of two potential xlists.
TCONS is a cons cell that points to two xlists.  If TNUM is 1, add ITEM
to the first xlist.  If TNUM is 2, add ITEM to the second xlist."
 (let (
  (x1    nil)
  (x2    nil)
      )
  (when (not tcons)
    (setq tcons (cons nil nil)))
  (if (= tnum 1)
    (progn
      (setq x1 (car tcons))
      (setq x1 (shu-add-to-xlist x1 item))
      (setcar tcons x1))
    (if (= tnum 2)
      (progn
        (setq x2 (cdr tcons))
        (setq x2 (shu-add-to-xlist x2 item))
        (setcdr tcons x2))
      (error "tnum is neither 1 nor 2.")))
 tcons
))

;;
;;  shu-add-to-xlist
;;
(defun shu-add-to-xlist (tlist item)
  "Add an item to the tail of an xlist.
car of 1st element points to a list of properties, the first of which is
a pointer to the tail of the list.  The second item in the properties list
is the point at which the visible scan of the buffer is to resume."
 (let (
;  (lbuf (get-buffer-create "*foo*"))
  (new1    (cons item nil))       ; cons cell for new (or perhaps only) item
  (props   nil)
  (last1   nil)
      )
  (if (not tlist)                 ; List is currently empty
    (progn
      (setq props (list new1 -1))     ; car points to list of properties
      (setq tlist (cons props new1))) ; cdr points to first item on list
                                  ; List is not empty
    (setq props (car tlist))      ; Get properties
    (setq last1 (car props))      ; car of properties points to last one
    (setcdr last1 new1)           ; Last one points forward to new one
    (setcar props new1))          ; car of properties points to new last one
;(print tlist (get-buffer lbuf))
  tlist
))

;;
;;  shu-set-next-visible-xc
;;
(defun shu-set-next-visible-xc (tcons vstart)
  "Set the next visible start point in two xlists."
 (let (
  (x1    nil)
  (x2    nil)
      )
  (when (not tcons)
    (setq tcons (cons nil nil)))
  (setq x1 (car tcons))
  (setq x1 (shu-set-next-visible x1 vstart))
  (setcar tcons x1)
  (setq x2 (cdr tcons))
  (setq x2 (shu-set-next-visible x2 vstart))
  (setcdr tcons x2)
 tcons
))

;;
;;  shu-set-next-visible
;;
(defun shu-set-next-visible (tlist vstart)
 (let (
;  (lbuf (get-buffer-create "*foo*"))
  (props    nil)
      )
  (when (not tlist)                 ; List is currently empty
    (setq props (list nil -1))
    (setq tlist (cons props nil))
    (setcar props tlist))
  (setq props    (car tlist))
  (setcar (cdr props) vstart)
; (print tlist (get-buffer lbuf))
 tlist
))

;;
;;  shu-next-visible
;;
(defun shu-next-visible (tlist)
 (let* (
;  (lbuf (get-buffer-create "*foo*"))
  (props    (car tlist))
  (nvis     (car (cdr props)))
      )
 nvis
))


;;; shu-visible.el ends here
