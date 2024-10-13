;;; shu-nvplist.el --- Shu project code for parsing name, value pair lists
;;
;; Copyright (C) 2013 Stewart L. Palmer
;;
;; Package: shu-nvplist
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
;;

;;; Commentary:

;; elisp code for maintaining directories of name / value pairs.
;;
;; A set of functions to parse a buffer of sets of name value pairs.
;;
;; Each set of name value pairs is enclosed in opening and closing delimiters.
;;
;; The opening delimiter is \"<\".  The closing delimiter is \"/>\".
;;
;; Each name value pair is a name followed by an equal sign followed by a value.
;;
;; This is an example of one set of name value pairs:
;;
;;       < id=mumble  bob=happy  car=\"Jaguar XKE\"  />
;;
;; If a name or a value contains spaces, it must be enclosed in quotes.
;;
;; These functions parse a buffer of sets of name value pairs into a list of
;; cons cells.
;;
;; The CAR of each cons cell is the item number.  The CDR of each cons cell is
;; the list of name value pairs.  Each name value pair is held on a cons cell
;; whose CAR is the name and whose CDR is the value.
;;
;; For example, the following set of name value pairs
;;
;;           <id=mumble  try=fuumble car=\"Jaguar XKE\" />
;;           <cat=dog  bob=happy />
;;           <mary=lamb  stew=bubble fox=hen   />
;;
;; would be converted to
;;
;;       (1 (id . mumble) (try . fuumble) (car . Jaguar XKE))
;;       (2 (cat . dog) (bob . happy))
;;       (3 (mary . lamb) (stew . bubble) (fox . hen))
;;
;;
;; Note that there is nothing in this mechanism that enforces name uniqueness in
;; a name value pair list.
;;
;; The following set of name value pairs
;;
;;     <id=mumble  try=fuumble car=\"Jaguar XKE\" id=bumble car=Audi/>
;;     <cat=dog  cat=tiger bob=happy />
;;     <mary=lamb  stew=bubble fox=hen fox=egg fox=smile  />
;;
;; would be converted to
;;
;;     (1 (id . mumble) (try . fuumble) (car . Jaguar XKE) (id . bumble)
;;        (car . Audi))
;;     (2 (cat . dog) (cat . tiger) (bob . happy))
;;     (3 (mary . lamb) (stew . bubble) (fox . hen) (fox . egg) (fox . smile))
;;
;; If one were to extract the value of \"car\" from the first list with
;; SHU-NVPLIST-GET-ITEM-VALUE, a list of length 2 would be returned that
;; is (list \"Jaguar XKE\" \"Audi\").
;;
;; Fetching the value of \"fox\" from the third list with
;; SHU-NVPLIST-GET-ITEM-VALUE would return
;; (list \"hen\" \"egg\" \"smile\")
;;
;; Fetching the value of \"mary\" from the third list with
;; SHU-NVPLIST-GET-ITEM-VALUE would return (list \"lamb\").
;;
;;
;; ```
;;  nvplist:
;;
;;   is a list of nvpair, each of which contains:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Value
;;        |
;;        +-------------> Name
;;
;;
;;  item:
;;
;;   -------------------
;;   |        |        |
;;   | Item # |   o-------------->nvplist
;;   |        |        |
;;   -------------------
;; ```
;;

;;; Code:


(require 'shu-base)

;;
;; shu-nvplist.el
;;

;;
;; shu-nvplist-parse-file
;;
(defun shu-nvplist-parse-file (file-name file-type item-list)
  "Parse a file full of name value pair lists.  The name of the file is
FILE-NAME.  The type of the file (only for error messages) is FILE-TYPE.
ITEM-LIST is the head of the returned item list."
  (let
      ((pwbuf  (find-buffer-visiting file-name))
       (cbuf   (current-buffer))
       (pwp)
       (pwbeg)
       (pwend)
       (we-opened-file))
    (save-selected-window
      (progn
        (save-current-buffer
          (save-excursion
            (when (not pwbuf)
              (setq we-opened-file t)
              (find-file file-name)
              (setq pwbuf (find-buffer-visiting file-name)))
            (if (not pwbuf)
                (progn
                  (message "%s file (%s) not found." file-type file-name)
                  (ding))
              (set-buffer pwbuf)
              (setq item-list (shu-nvplist-parse-buffer item-list))
              (when we-opened-file
                (shu-kill-current-buffer)))))))
    (switch-to-buffer cbuf))
  item-list
  )


;;
;;  shu-nvplist-parse-buffer
;;
(defun shu-nvplist-parse-buffer(item-list)
  "Parse an nvplist buffer, putting all of the items in the ITEM-LIST."
  (let
      ((gbuf      (get-buffer-create shu-unit-test-buffer))
       (opener (concat "\\s-*" "<" "\\s-*"))
       (tlist    )
       (zlist    )
       (nvplist   )
       (nvpair   )
       (line-col )
       (index    )
       (item     )
       (item-number  0)
       (show-list nil)
       (show-token-list))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward opener nil t)
          (save-excursion
            (let*
                ((bol (save-excursion (beginning-of-line) (point)))
                 (eol (save-excursion (end-of-line) (point)))
                 (line-no (shu-the-line-at bol)))))

          (setq item-number (1+ item-number))
          ;;
          ;; Make the token list, which is simply a list of tokens found in the item without
          ;; any meaning attributed to any token.  The equals sign is just another token
          ;; at this point.
          (setq tlist nil)
          (setq tlist (shu-nvplist-make-token-list tlist))
          (when show-token-list
            (setq zlist tlist)
            (while zlist
              (setq nvpair (car zlist))
              (setq line-col (shu-line-and-column-at (car nvpair)))
              (princ (format "Token at line %d (%d)  = <%s>\n" (car line-col) (cdr line-col) (cdr nvpair))  gbuf)
              (setq zlist (cdr zlist))))
          ;; Now turn the token list into a list of name value pairs.  Each item in the list is
          ;; an nvpair whose CAR is the name and whose CDR is the value.
          (setq nvplist (shu-nvplist-make-nvpair-list tlist))
          ;; An item is a CONS cell whose CAR is the item number and whose CDR is the nvpair list.
          (setq item (shu-nvplist-make-item item-number nvplist))
          (when show-list
            (setq zlist nvplist)
            (while zlist
              (setq nvpair (car zlist))
              (setq zlist (cdr zlist))))
          ;;
          ;;
          (setq item-list (cons item item-list)))))
    item-list
    ))

;;
;;  shu-nvplist-make-item
;;
(defun shu-nvplist-make-item (item-number nvplist)
  "Create an item entry from an item number and a name value pair list.
The item entry is just a cons cell with the item number in the CAR and the
name-value pair list in the CDR."
  (let
      ((item  (cons item-number nvplist)))
    item
    ))

;;
;;  shu-nvplist-get-item-number
;;
(defun shu-nvplist-get-item-number (item)
  "Return the item number for an item."
  (let
      ((item-number (car item)))
    item-number
    ))

;;
;;  shu-get-item-nvplist
;;
(defun shu-get-item-nvplist (item)
  "Return the name value pair list from an item."
  (let
      ((nvplist (cdr item)))
    nvplist
    ))



;;
;;  shu-nvplist-show-item-list
;;
(defun shu-nvplist-show-item-list (item-list)
  (let
      ((tlist item-list)
       (item))
    (while tlist
      (setq item (car tlist))
      (shu-nvplist-show-item item)
      (setq tlist (cdr tlist)))
    ))

;;
;;  shu-nvplist-show-item
;;
(defun shu-nvplist-show-item (item)
  (let (
        (gbuf      (get-buffer-create shu-unit-test-buffer))
        (item-number  )
        (nvplist       )
        (nvpair       )
        (name         )
        (value        )
        (vno         0)
        )
    (setq item-number (shu-nvplist-get-item-number item))
    (setq nvplist (shu-get-item-nvplist item))
    (princ (format "%3d: (" item-number) gbuf)
    (setq vno (1+ vno))
    (while nvplist
      (setq nvpair (car nvplist))
      (setq name (car nvpair))
      (setq value (cdr nvpair))
      (when (not (= vno 1))
        (princ "   " gbuf))
      (princ (concat name "=" value) gbuf)
      (setq vno (1+ vno))
      (setq nvplist (cdr nvplist)))
    (princ ")\n" gbuf)
    ))

;;
;;  shu-nvplist-get-item-value
;;
(defun shu-nvplist-get-item-value(name item)
  "Extract a named list of values from an item.  NAME is the name of the values
to find.  ITEM is the item from which to extract the values.  A list is returned
that contain all of the values whose name matches NAME."
  (let
      ((vlist     )
       (nvpair    )
       (vname     )
       (value     )
       (nvplist (shu-get-item-nvplist item)))
    (while nvplist
      (setq nvpair (car nvplist))
      (setq vname (car nvpair))
      (when (string= (upcase name) (upcase vname))
        (setq value (cdr nvpair))
        (setq vlist (cons value vlist)))
      (setq nvplist (cdr nvplist)))
    (setq vlist (nreverse vlist))
    vlist
    ))

;;
;;  shu-nvplist-make-nvpair-list
;;
(defun shu-nvplist-make-nvpair-list (tlist)
  "Turn a list of tokens from an entry in the file into a list of name value
pairs.  The CAR of each entry in the list is the name.  The CDR of each entry in
the list is the value.  If errors are found in the token list, then an empty
list is returned."
  (let (
        (gbuf      (get-buffer-create shu-unit-test-buffer))
        (nvplist       )
        (nvpair       )
        (zlist   tlist)
        (token-holder )
        (name         )
        (name-point   )
        (equal        )
        (equal-point  )
        (value        )
        (value-point  )
        (line-col     )
        )
    (while zlist
      (setq token-holder (pop zlist))
      (setq name-point (car token-holder))
      (setq name (cdr token-holder))

      (setq token-holder (pop zlist))
      (if token-holder
          (progn
            (setq equal-point (car token-holder))
            (setq equal (cdr token-holder))
            (if (string= equal "=")
                (progn
                  (setq token-holder (pop zlist))
                  (if token-holder
                      (progn
                        (setq value-point (car token-holder))
                        (setq value (cdr token-holder))
                        (setq nvpair (cons name value))
                        (setq nvplist (cons nvpair nvplist)))
                    ;; Value that should follow equal sign is missing
                    (setq line-col (shu-line-and-column-at name-point))
                    (princ
                     (format "The name (%s) at line %d column %d has no value.\n"
                             name (car line-col) (cdr line-col))  gbuf)
                    (setq zlist nil)
                    (setq nvplist nil)))
              ;; Token following name is not an equal sign
              (setq line-col (shu-line-and-column-at name-point))
              (princ
               (format "The token (%s) following the name (%s) at line %d column %d is not \"=\".\n"
                       equal name (car line-col) (cdr line-col))  gbuf)
              (setq zlist nil)
              (setq nvplist nil)))
        ;; Token that should follow name ("=") is not there at all
        (setq line-col (shu-line-and-column-at name-point))
        (princ
         (format "The name (%s) at line %d column %d is the last item in the entry.\n"
                 name (car line-col) (cdr line-col))  gbuf)
        (setq zlist nil)
        (setq nvplist nil)))
    (setq nvplist (nreverse nvplist))
    nvplist
    ))

;;
;;  shu-nvplist-make-token-list
;;
(defun shu-nvplist-make-token-list (tlist)
  "Turn an entry in a name / value file into a list of tokens.  The CAR of each
entry is the point at which the token starts.  the CDR of each entry in the list
is the token itself.  On entry to this function, point is immediately after the
start delimiter (\"<\").  On return, point is positioned immediately after the
end delimiter (\"/>\")."
  (let
      ((gbuf      (get-buffer-create shu-unit-test-buffer))
       (debug-on-error   t)
       (closer (regexp-opt (list "/>")))
       (string-start (regexp-opt (list "'" "\"") nil))
       (string-term  ) ;; Char that terminates the current string (single or double quote)

       (done     )
       (spoint   )
       (epoint   )
       (name     )
       (value    )
       (item     )
       (nvpair   )
       (limit    )
       (token    )
       (line-col )
       (show-list nil))

    (setq done nil)
    (while (not done)
      (skip-chars-forward shu-all-whitespace-regexp-scf)
      (cond
       ((looking-at string-start)
        (setq spoint (point))
        (setq string-term (buffer-substring (point) (1+ (point))))
        (setq epoint (shu-end-of-string string-term))
        (setq token (buffer-substring (1+ spoint) (1- epoint))))

       ((looking-at closer)
        (setq done t))

       ((looking-at "=")
        (setq token "=")
        (setq spoint (point))
        (goto-char (1+ (point))))

       (t   ;; Unquoted token
        (setq spoint (point))
        (save-excursion
          (setq limit nil)
          (when (re-search-forward closer nil t)
            (setq limit (match-beginning 0))))
        (skip-chars-forward "^ \t\n=" limit)
        (setq epoint (point))
        (setq token (buffer-substring spoint epoint))))
      (when (not done)
        (setq nvpair (cons spoint token))
        (setq tlist (cons nvpair tlist))))
    (setq tlist (nreverse tlist))
    (when show-list
      (while tlist
        (setq nvpair (car tlist))
        (setq line-col (shu-line-and-column-at (car nvpair)))
        (princ (format "Token at line %d (%d)  = <%s>\n" (car line-col) (cdr line-col) (cdr nvpair))  gbuf)
        (setq tlist (cdr tlist))))
    tlist
    ))



;;
;;  shu-nvplist-get-names
;;
(defun shu-nvplist-get-names (item)
  "Return a list of all of the distinct names from an NVPLIST."
  (let ((nvplist (shu-get-item-nvplist item))
        (vname)
        (uvname)
        (uname-list)
        (name-list))
    (while nvplist
      (setq vname (caar nvplist))
      (setq uvname (upcase vname))
      (unless (member uvname uname-list)
        (push uvname uname-list)
        (push vname name-list))
      (setq nvplist (cdr nvplist)))
    (setq name-list (sort name-list (lambda(t1 t2) (string< (upcase t1) (upcase t2)))))
    name-list
    ))



;;
;;  shu-nvplist-quoted-value
;;
(defun shu-nvplist-quoted-value (value)
  "Return VALUE as a quoted string if it contains any characters that would have
required it to be quoted in the original source list.  Otherwise return VALUE
unaltered."
  (let ((quoted-value value)
        (delims " =/>"))
        (when (shu-any-char-from-first-in-second delims value)
          (setq quoted-value (concat "\"" quoted-value "\"")))
        quoted-value
    ))



;;
;;  shu-nvplist-to-string
;;
(defun shu-nvplist-to-string (nvplist)
  "Return an NVPLIST as a atring."
  (let ((nvpair)
        (name)
        (value)
        (sepchar "")
        (string-value ""))
    (while nvplist
      (setq nvpair (car nvplist))
      (setq name (car nvpair))
      (setq value (shu-nvplist-quoted-value (cdr nvpair)))
      (when (string= (upcase name) "PW")
        (setq value "........"))
      (setq string-value (concat string-value sepchar name "=" value))
      (setq sepchar " ")
      (setq nvplist (cdr nvplist)))
    string-value
    ))



;;
;;  shu-nvplist-item-to-string
;;
(defun shu-nvplist-item-to-string (item)
  "Return an ITEM (and its NVPLIST) as a string."
  (let (
        (item-number (shu-nvplist-get-item-number item))
        (pitem-number)
        (nvplist (shu-get-item-nvplist item))
        (string-value "")
        )
    (setq pitem-number (shu-fixed-format-num item-number 8))
    (concat pitem-number ": " (shu-nvplist-to-string nvplist))
    ))




;;
;;  shu-nvplist-check-duplicates
;;
(defun shu-nvplist-check-duplicates (keys item buffer-name)
  "ITEM is an ITEM produced by SHU-NVPLIST-PARSE-BUFFER.  The ITEM contains an
NVPLIST.  KEYS is a list of names each of which is only allowed to have no more
that one occurrence in the NVPLIST.  The NVPLIST mechanism does not constrain
the number of times that a name may appear in a name value pair.  But a web site
can have only one \"id\" and one \"pw\".  If the NVPLIST is used by
SHU-KEYRING.EL, it makes no sense to have more than one instance of either
\"id\" or \"pw\" in a given NVPLIST.

The return value of this function is a count of the number of names in KEYS that
occur more than once in the NVPLIST.  If the names in KEYS may appear at most
once in a given NVPLIST, then the returned duplicate count must be zero if the
NVPLIST meets the criteria of not having duplicate entries for the names
specified in KEYS.

BUFFER-NAME is the name of the buffer into which error messages are emitted if
duplicate names are detected."
  (let ((gb (get-buffer-create buffer-name))
        (key-list keys)
        (dup-count 0)
        (key)
        (vlist)
        (item-number (shu-nvplist-get-item-number item)))
    (while key-list
      (setq key (car key-list))
      (setq vlist (shu-nvplist-get-item-value key item))
      (when (> (length vlist) 1)
        (setq dup-count (1+ dup-count))
        (princ (format "Item %d has %d instances of \"%s\"\n" item-number (length vlist) key) gb))
      (setq key-list (cdr key-list)))
    dup-count
    ))

(provide 'shu-nvplist)

;;; shu-nvplist.el ends here
