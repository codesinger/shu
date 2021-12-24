;;; shu-keyring.el --- Shu project code for dealing with encrypted keyrings
;;
;; Copyright (C) 2013 Stewart L. Palmer
;;
;; Package: shu-keyring
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

;; This is a set of functions for maintaining and querying a keyring of names,
;; URLs, users IDs, passwords, and related information that are maintained in an
;; external keyring file.
;;
;; Functions allow you to find a keyring entry by name and to put one piece of
;; its information, such as user ID or password, in the clip board, from which it
;; may be pasted into a browser or other application.
;;
;; The keyring file may be encrypted with GPG.  As of emacs 23, the EasyPG
;; package is included with the emacs distribution.  When you tell emacs to open
;; a file that has been encrypted with GPG, you are prompted for the passphrase
;; and the file is decrypted into memory.
;;
;; The file keyring.txt in the usr directory is am example of a small
;; keyring file that has not been encrypted.  Each entry in the file consists of
;; a set of name value pairs.  Each value may be enclosed in quotes and must be
;; enclosed in quotes if it contains embedded blanks.
;;
;; A single set of name value pairs starts with an opening "<" and is terminated
;; by a closing "/>".
;;
;; Here is an example of a set of name value pairs:
;;
;;      < name="Fred email" url=mail.google.com  id=freddy@gmail.com  pw=secret />
;;
;; The names may be arbitrary but there are six names that are recognized by the
;; functions in this package.  They are:
;;
;; acct represents an account number
;;
;; id represents a user ID
;;
;; name represents the name of the entry.  This is the key that is used to find
;; the entry.  If no name is given, then the name of the URL is used.  If the URL
;; starts with "www.", the "www." is removed to form the name.  An entry that has
;; no name and a URL of "www.facebook.com" would have an auto generated name of
;; "facebook.com".
;;
;; pin represents a pin number
;;
;; pw represents a password
;;
;; url represents a URL
;;
;; To use a keying file, place the following lines in your .emacs file:
;;
;;      (load-file "/Users/fred/.emacs.d/shu-base.elc")
;;      (load-file "/Users/fred/.emacs.d/shu-nvplist.elc")
;;      (load-file "/Users/fred/.emacs.d/shu-keyring.elc")
;;      (shu-keyring-set-alias)
;;      (setq shu-keyring-file "/Users/fred/shu/usr/keyring.txt")
;;
;; replacing "/Users/fred/shu/usr/keyring.txt" with the path to your keyring file.
;;
;; All of the shu functions require shu-base.
;;
;; If using the sample keyring file, Fred can now use this to log onto his gmail
;; account as follows.
;;
;; Type M-x krurl.  This prompts for the name of the desired key.  Type "Fred em"
;; and hit TAB to complete.  This fills out the name as "Fred email" and puts the
;; URL "mail.google.com" into the clip board.  Open a browser and paste the URL
;; into it to go to gmail.  At gmail, select login.  In emacs type M-x krid.
;; When prompted for the key, use the up arrow to retrieve the last key used,
;; which will be "Fred email".  This puts "freddy@gmail.com" into the clip board
;; for conveniently pasting into the gmail widow.  To obtain the password, type
;; M-x krpw.  This puts the password into the clip board from which it may be
;; pasted into the gmail widow.

;;; Code:


(require 'shu-nvplist)

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
;;
;;
;;  index:
;;
;;   is a list of key-item, each of which contains:
;;
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> item
;;        |
;;        +-------------> Key
;;
;;  Given a key value, one can use (assoc), (assq), (assoc-default), etc. to find
;;  the item associated with the key.
;; ```
;;



(defgroup shu-keyring nil
  "Maintain a keyring file of user IDs, URLs and passwords."
  :group 'applications)

(defcustom shu-keyring-file "~/plist.txt"
  "Text file in which urls, names, and passwords are stored."
  :type '(string)
  :group 'shu-keyring)


(defvar shu-keyring-index nil
  "The variable that points to the in-memory keyring index.")

(defvar shu-keyring-history   nil
  "The history list used by completing-read when asking the user for a key to an
entry in the keyring file.")

(defconst shu-keyring-url-name   "url"
  "Key word that denotes a URL.")

(defconst shu-keyring-name-name   "name"
  "Key word that denotes a name.")

(defconst shu-keyring-account-name   "account"
  "Key word that denotes a name.")

(defconst shu-keyring-id-name   "id"
  "Key word that denotes a user ID.")

(defconst shu-keyring-pw-name   "pw"
  "Key word that denotes a password.")

(defconst shu-keyring-pin-name   "pin"
  "Key word that denotes a PIN.")

(defconst shu-keyring-buffer-name  "**shu-keyring**"
  "The name of the buffer into which keyring diagnostics and messages
are recorded.")

(defvar shu-keyring-external-passphrase nil
  "Holds the external passphrase for the keyring file.  This allows the user
to type the passphrase at the beginning of an emacs session.  Once this is
set it can then be put into kill ring by shu-keyring-get-passphrase.")


;;
;; shu-keyring-get-pw
;;
(defun shu-keyring-get-pw()
  "Find the password for an entry in the keyring file.  This displays the entry
(without the password) in the message area and puts the password into the kill
ring so that it can be yanked into a buffer or pasted into the application
requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-pw-name)
  )

;;
;; shu-keyring-get-pin
;;
(defun shu-keyring-get-pin()
  "Find the pin for an entry in the keyring file.  This displays the entry in
the message area and puts the pin into the kill ring so that it can be yanked
into a buffer or pasted into the application requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-pin-name)
  )

;;
;; shu-keyring-get-id
;;
(defun shu-keyring-get-id()
  "Find the User Id for an entry in the keyring file.  This displays the entry
in the message area and puts the user Id into the kill ring so that it can be
yanked into a buffer or pasted into the application requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-id-name)
  )

;;
;; shu-keyring-get-url
;;
(defun shu-keyring-get-url()
  "Find the url for an entry in the keyring file.  This displays the entry in
the message area and puts the url into the kill ring so that it can be yanked
into a buffer or pasted into the application requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-url-name)
  )

;;
;; shu-keyring-get-acct
;;
(defun shu-keyring-get-acct()
  "Find the account for an entry in the keyring file.  This displays the entry in the message
area and puts the password into the kill ring so that it can be yanked or pasted into the application
requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-account-name)
  )


;;
;; shu-keyring-get-file
;;
(defun shu-keyring-get-file()
  "Display the name of the keyring file, if any.  This is useful if you are
getting unexpected results from some of the query functions that look up keyring
information.  Perhaps the unexpected results come from the fact that you are
using the wrong keyring file."
  (interactive)
  (if shu-keyring-file
      (message "Shu keyring file is \"%s\"" shu-keyring-file)
    (message "%s" "No keyring file is defined."))
  )

;;
;;  shu-keyring-clear-index
;;
(defun shu-keyring-clear-index()
  "This is called from after-save-hook to clear the keyring index if the keyring file is saved.
The keyring index is built the first time it is needed and kept in memory thereafter.  But we
must refresh the index if the keyring file is modified.  The easiest way to do this is to clear
the index when the keyring file is modified.  The next time the index is needed it will be
recreated."
  (let
      ((fn1 (buffer-file-name))
       (fn2 (abbreviate-file-name (buffer-file-name))))

    (when (or (string= shu-keyring-file fn1)
              (string= shu-keyring-file fn2))
      (setq shu-keyring-index nil))
    ))


;;
;;  shu-keyring-verify-file
;;
(defun shu-keyring-verify-file ()
  "Parse and verify the keyring file, displaying the result of the operation in the
keyring buffer (**shu-keyring**).  If one of the queries for a url or other
piece of information is unable to find the requested information, it could be
that you have the wrong keyring file or that there is a syntax error in the
keyring file.  shu-keyring-get-file (alias krfn) displays the name of the
keyring file.  This function parses the keyring file.  After the operation. look
into the keyring buffer (**shu-keyring**) to see if there are any complaints
about syntax errors in the file."
  (interactive)
  (setq shu-keyring-index nil)
  (when (bufferp shu-keyring-buffer-name)
    (kill-buffer shu-keyring-buffer-name))
  (shu-keyring-parse-keyring-file)
  (switch-to-buffer shu-keyring-buffer-name)
  (goto-char (point-min))
  )



;;
;;  shu-keyring-get-field
;;
(defun shu-keyring-get-field (name)
  "Fetch the value of a named field from the keyring.  Prompt the user with a completing-read
for the field that identifies the key.  Use the key to find the item.  Find the value of the named
key value pair within the item.  Put the value in the kill-ring and also return it to the caller."
  (let ((gbuf      (get-buffer-create shu-keyring-buffer-name))
        (invitation   "Key? ")
        (keyring-key   )
        (keyring-entry )
        (item          )
        (vlist         )
        (item-value    ))
    (when (not shu-keyring-index)
      (shu-keyring-parse-keyring-file))
    (if (not shu-keyring-index)
        (progn
          (message "Could not parse keyring.  See %s." shu-keyring-buffer-name)
          (ding))

      (let
          ((completion-ignore-case t))
        (setq keyring-key
              (completing-read
               invitation  ;; prompt
               shu-keyring-index   ;; collection
               nil         ;; predicate function
               nil         ;; require-match
               nil         ;; Initial input (initial minibuffer contents)
               shu-keyring-history  ;; History list
               nil)))      ;; Default value
      (setq keyring-entry  (assoc keyring-key shu-keyring-index))
      (setq item (cdr keyring-entry))
      (shu-keyring-show-name-url name item)
      (setq vlist (shu-nvplist-get-item-value name item))
      (if (not vlist)
          (ding)
        (setq item-value (car vlist))))
    (if item-value
        (kill-new item-value)
      (kill-new "unknown"))
    item-value
    ))


;;
;; shu-keyring-parse-keyring-file
;;
(defun shu-keyring-parse-keyring-file ()
  "Parse the keyring file and create the in-memory index if the keyring file
contains no duplicate keys."
  (interactive)
  (let
      ((gbuf      (get-buffer-create shu-keyring-buffer-name))
       (count        )
       (file-type   "Keyring")
       (item-list    )
       (ilist        )
       (item         )
       (index        ))
    (princ (format "Shu keyring file is \"%s\".\n" shu-keyring-file) gbuf)
    (setq item-list (shu-nvplist-parse-file shu-keyring-file file-type item-list))
    (setq ilist item-list)
    (while ilist
      (setq item (car ilist))
      (setq index (shu-keyring-update-index index item))
      (setq ilist (cdr ilist)))
    (setq index (sort index (lambda(t1 t2) (string< (upcase (car t1)) (upcase (car t2))))))
    (if (shu-keyring-find-index-duplicates index)
        (princ "Index has duplicates\n" gbuf)
      (princ "Index has no duplicates\n" gbuf)
      (setq count (length index))
      (setq shu-keyring-index index)
      (princ (format "Index contains %d entries.\n" count)  gbuf))
    (shu-keyring-show-index index)
    ))


;;
;;  shu-keyring-show-index
;;
(defun shu-keyring-show-index (index)
  "Print the keyring index"
  (let
      ((gbuf      (get-buffer-create shu-keyring-buffer-name))
       (tindex  index)
       (key-item )
       (key     )
       (item    )
       (item-number )
       (item-number-string )
       (count   0))
    (princ "     Line\n" gbuf)
    (princ "    Number   Key ...\n" gbuf)
    (while tindex
      (setq count (1+ count))
      (setq key-item (car tindex))
      (setq key  (car key-item))
      (setq item (cdr key-item))
      (setq item-number (shu-nvplist-get-item-number item))
      (setq item-number-string (shu-fixed-format-num item-number 10))
      (princ (format "%s:  %s\n" item-number-string key) gbuf)
      (setq tindex (cdr tindex)))
    (princ (format "Index contains %d entries.\n" count)  gbuf)
    ))



;;
;;  shu-keyring-find-index-duplicates
;;
(defun shu-keyring-find-index-duplicates (index)
  "Find any duplicates in the keyring index.  When the index is built we filter duplicate
keys for the same item.  But there could be two different items with the same key.  This
function returns TRUE if two or more items have the same key.  The index must be in sorted
order by key value before this function is called."
  (let
      ((gbuf      (get-buffer-create shu-keyring-buffer-name))
       (tindex  index)
       (key-item )
       (key     )
       (item    )
       (item-number )
       (last-key   nil)
       (last-item-number  0)
       (has-duplicate     nil))
    (while tindex
      (setq key-item (car tindex))
      (setq key  (car key-item))
      (setq item (cdr key-item))
      (setq item-number (shu-nvplist-get-item-number item))
      (when last-key
        (when (string= (upcase last-key) (upcase key))
          (setq has-duplicate t)
          (if (= item-number last-item-number)
              (princ (format "Item %d has duplicate key <%s>\n" item-number key) gbuf)
            (princ (format "Items %d and %d share key <%s>\n" last-item-number item-number key) gbuf))))
      (setq last-key key)
      (setq last-item-number item-number)
      (setq tindex (cdr tindex)))
    has-duplicate
    ))

;;
;;  shu-keyring-update-index
;;
(defun shu-keyring-update-index (index item)
  "Extract the keys from a keyring item and add them to the keyring index."
  (let
      ((vlist   )
       (tlist   )
       (value   )
       (new-value ))
    ;; First add all the names for this item to the index
    (setq vlist (shu-nvplist-get-item-value "name" item))
    (setq index (shu-keyring-add-values-to-index index vlist item))

    ;; Now get all the urls
    (setq vlist (shu-nvplist-get-item-value "url" item))
    (setq tlist vlist)

    ;; For each url of the form "www.something", we also add a key of "something"
    ;; This may result in duplicates.  But the function that actually adds the values
    ;; to the index eliminates duplicates.
    (while tlist
      (setq value (car tlist))
      (when (string= (substring (upcase value) 0 4) "WWW.")
        (setq new-value (substring value 4))
        (setq vlist (cons new-value vlist)))
      (setq tlist (cdr tlist)))
    (setq index (shu-keyring-add-values-to-index index vlist item))
    index
    ))


;;
;;  shu-keyring-in-index
;;
(defun shu-keyring-in-index (index item value)
  "Return true if the INDEX already contains the VALUE for this ITEM."
  (let
      ((item-number (shu-nvplist-get-item-number item))
       (tindex  index)
       (done         )
       (pair         )
       (xvalue       )
       (xitem        )
       (xitem-number )
       (got-it    nil))
    (when (not tindex)
      (setq done t))
    (while (not done)
      (setq pair (car tindex))
      (setq xvalue (car pair))
      (setq xitem (cdr pair))
      (setq xitem-number (shu-nvplist-get-item-number xitem))
      (when (and (= item-number xitem-number) (string= xvalue value))
        (setq done t)
        (setq got-it t))
      (setq tindex (cdr tindex))
      (when (not tindex)
        (setq done t)))
    got-it
    ))


;;
;;  shu-keyring-add-values-to-index
;;
(defun shu-keyring-add-values-to-index (index vlist item)
  "Add a set of keys VLIST to INDEX for ITEM.  Keys within the item are filtered for
duplicates.  But this does not prevent two different items from sharing the same key,
although it would be unusual in a keyring."
  (let
      ((zlist   vlist)
       (value   )
       (pair    ))
    (while zlist
      (setq value (car zlist))
      (when (not (shu-keyring-in-index index item value))
        (setq pair (cons value item))
        (setq index (cons pair index)))
      (setq zlist (cdr zlist)))
    index
    ))


;;
;;  shu-keyring-show-name-url
;;
(defun shu-keyring-show-name-url (type item)
  "Show in the message area the name, url, or both of a keyring entry.  Also prefix
the message with the upper case type, which is the type of the entry that has been
placed in the clipboard, (PW, ID, etc.)"
  (let ((names   (shu-nvplist-get-item-value shu-keyring-name-name item))
        (urls    (shu-nvplist-get-item-value shu-keyring-url-name item))
        (ids     (shu-nvplist-get-item-value shu-keyring-id-name item))
        (mstring    "")
        (got-string  t))
    (cond
     ((and (not urls) (not names)) ;; Neither names nor urls
      ;; Nothing to display
      (setq got-string nil))
     ((and names (not urls)) ;; names only
      (setq mstring (shu-keyring-values-to-string names)))
     ((and urls (not names)) ;; urls only
      (setq mstring  (shu-keyring-values-to-string urls)))
     (t                       ;; Both names and urls
      (setq mstring (concat (shu-keyring-values-to-string urls) " (" (shu-keyring-values-to-string names) ")"))))
    (when got-string
      (setq mstring (concat mstring " ")))
    (when ids
      (setq mstring (concat mstring "id=" (shu-keyring-values-to-string ids))))
    (setq mstring (concat (upcase type) ": " mstring))
    (message "%s" mstring)
    ))


;;
;;  shu-keyring-values-to-string
;;
(defun shu-keyring-values-to-string (values)
  "Turn a list of values into a single string of values separated by slashes."
  (let ((retval "")
        (x       0))
    (while values
      (setq x (1+ x))
      (when (not (= x 1))
        (setq retval (concat retval " / ")))
      (setq retval (concat retval (car values)))
      (setq values (cdr values)))
    retval
    ))


;;
;;  shu-keyring-set-passphrase
;;
(defun shu-keyring-set-passphrase (phrase)
  "Function to read and set the external pass phrase."
  (interactive "sPass phrase?: ")
  (setq shu-keyring-external-passphrase phrase)
  )


;;
;;  shu-keyring-get-passphrase
;;
(defun shu-keyring-get-passphrase ()
  "Doc string."
  (interactive)
  (let ((phrase "**unknown**"))
    (if shu-keyring-external-passphrase
        (setq phrase shu-keyring-external-passphrase)
      (ding)
      (message "%s" "Pass phrase is not set."))
    (shu-kill-new phrase)
    ))


;;
;;  shu-keyring-set-alias
;;
(defun shu-keyring-set-alias ()
  "Set the common alias names for the functions in shu-keyring.
These are generally the same as the function names with the leading
shu- prefix removed.  But in this case the names are drastically shortened
to make them easier to type. "
  (defalias 'krpw 'shu-keyring-get-pw)
  (defalias 'krurl 'shu-keyring-get-url)
  (defalias 'krpin 'shu-keyring-get-pin)
  (defalias 'krid 'shu-keyring-get-id)
  (defalias 'kracct 'shu-keyring-get-acct)
  (defalias 'krfn 'shu-keyring-get-file)
  (defalias 'krvf 'shu-keyring-verify-file)
  (defalias 'krpps 'shu-keyring-get-passphrase)
  (defalias 'set-passphrase 'shu-keyring-set-passphrase)
  )

;;; shu-keyring.el ends here
