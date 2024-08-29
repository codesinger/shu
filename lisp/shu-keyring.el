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
;; enclosed in quotes if it contains embedded blanks, commas, or slashes.
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
;; route represents a bank routing number
;;
;; pw represents a password
;;
;; oldpw represents an expired password
;;
;; url represents a URL
;;
;; To use a keying file, place the following lines in your .emacs file:
;;
;;      (load-file "/Users/fred/.emacs.d/shu-base.elc")
;;      (load-file "/Users/fred/.emacs.d/shu-nvplist.elc")
;;      (load-file "/Users/fred/.emacs.d/shu-nvpindex.elc")
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
;;
;; The interctive commands defined in this file are as follows:
;;
;; krpw    - Fetch the password for an entry
;; kroldpw - Fetch the expired password for an entry
;; krurl   - Fetch the URL for an entry
;; krpin   - Fetch the PIN for an entry
;; krid    - Fetch the Use ID for an entry
;; kracct  - Fetch the account number for an entry
;; krrt    - Fetch the bank routing number for an entry
;; krnewpw - Generate a new, random password
;; krfn    - Display the name of the keyring file
;; krvf    - Parse and verify the keyring file
;; krpps   - Fetch the global passphrase
;; kraps   - Fetch the global alternate passphrase
;; set-passphrase - Set the global passphrase
;; set-alternate-passphrase - Set the global alternate passphrase

;;; Code:


(require 'shu-nvplist)
(require 'shu-nvpindex)
(require 'shu-misc)

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

(defvar shu-keyring-last-key nil
  "The variable that holds the last key read from the minibuffer.  This is used
 as the default value for the next completing-read")

(defconst shu-keyring-url-name   "url"
  "Key word that denotes a URL.")

(defconst shu-keyring-name-name   "name"
  "Key word that denotes a name.")

(defconst shu-keyring-account-name   "account"
  "Key word that denotes an account name.")

(defconst shu-keyring-route-name   "route"
  "Key word that denotes a bank routing number.")

(defconst shu-keyring-id-name   "id"
  "Key word that denotes a user ID.")

(defconst shu-keyring-pw-name   "pw"
  "Key word that denotes a password.")

(defconst shu-keyring-oldpw-name   "oldpw"
  "Key word that denotes an expired password.")

(defconst shu-keyring-pin-name   "pin"
  "Key word that denotes a PIN.")

(defconst shu-keyring-buffer-name  "**shu-keyring**"
  "The name of the buffer into which keyring diagnostics and messages
are recorded.")

(defconst shu-keyring-file-type "Keyring"
  "The name of the file type of keyring file.  Used for diagnostic messages.")

(defvar shu-keyring-external-passphrase nil
  "Holds the external passphrase for the keyring file.  This allows the user
to type the passphrase at the beginning of an emacs session.  Once this is
set it can then be put into kill ring by shu-keyring-get-passphrase.")

(defvar shu-keyring-alternate-passphrase nil
  "Holds the alternate passphrase.  The alternate passphrase has no assigned
meaning.  It means whatever the user wants it to mean.")


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
;; shu-keyring-get-oldpw
;;
(defun shu-keyring-get-oldpw()
  "Find the expired password for an entry in the keyring file.  This displays
the entry (without the password) in the message area and puts the expired
password into the kill ring so that it can be yanked into a buffer or pasted
into the application requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-oldpw-name)
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
  "Find the account for an entry in the keyring file.  This displays the entry
in the message area and puts the account number into the kill ring so that it
can be yanked or pasted into the application requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-account-name)
  )

;;
;; shu-keyring-get-route
;;
(defun shu-keyring-get-route()
  "Find the routing number for an entry in the keyring file.  This displays the
entry in the message area and puts the routing number into the kill ring so that
it can be yanked or pasted into the application requesting it."
  (interactive)
  (shu-keyring-get-field shu-keyring-route-name)
  )


;;
;;  shu-keyring-clear-last
;;
(defun shu-keyring-clear-last ()
  "Overwrite the last item placed in the kill ring.  The current item in the
kill ring will be the thing that is pasted by an operating system paste
function.  If the last thing placed in the kill ring is a password that you do
not want arbitrarily pasted again, you can call this function to add a string to
kill ring.  The password remains in the kill ring, but will not be pasted again
by the next operating system paste operation because it is no longer the current
item in the kill ring."
  (interactive)
  (let ((nothing "***NONE***"))
    (shu-kill-new nothing)
    ))


;;
;;  shu-keyring-generate-password
;;
(defun shu-keyring-generate-password (length-range)
  "Generate a random password and put it in the kill ring.  Issue a query for
the minimum and maximum password length.  If two numbers are given, these are
the minimum and maximum password lengths.  If only one number is given, this is
the exact password length.  The password is created by calling
SHU-GENERATE-PASSWORD and putting the returned password into the kill ring."
 (interactive "sLength length or min.max?: ")
  (let ((range)
        (min-length)
        (max-length)
        (pw))
    (setq range (shu-split-range-string length-range))
    (setq min-length (car range))
    (setq max-length (cdr range))
    (setq pw (shu-generate-password min-length max-length))
    (shu-kill-new pw)
    ))


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
  "This is called from after-save-hook to clear the keyring index if the keyring
file is saved.  The keyring index is built the first time it is needed and kept
in memory thereafter.  But we must refresh the index if the keyring file is
modified.  The easiest way to do this is to clear the index when the keyring
file is modified.  The next time the index is needed it will be recreated."
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
  (setq shu-keyring-index
        (shu-nvpindex-parse-file shu-keyring-file shu-keyring-file-type shu-keyring-buffer-name))
  (switch-to-buffer shu-keyring-buffer-name)
  (goto-char (point-min))
  )



;;
;;  shu-keyring-get-field
;;
(defun shu-keyring-get-field (name)
  "Fetch the value of a named field from the keyring.  Prompt the user with a
completing-read for the field that identifies the key.  Use the key to find the
item.  Find the value of the named key value pair within the item.  Put the
value in the kill-ring and also return it to the caller."
  (let ((gbuf      (get-buffer-create shu-keyring-buffer-name))
        (invitation   "Key? ")
        (keyring-key   )
        (keyring-entry )
        (item          )
        (vlist         )
        (item-value    ))
    (when (not shu-keyring-index)
      (setq shu-keyring-index
            (shu-nvpindex-parse-file shu-keyring-file shu-keyring-file-type shu-keyring-buffer-name)))
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
               shu-keyring-last-key)))      ;; Default value
      (setq keyring-entry  (assoc keyring-key shu-keyring-index))
      (setq item (cdr keyring-entry))
      (shu-keyring-show-name-url name item)
      (setq vlist (shu-nvplist-get-item-value name item))
      (if (not vlist)
          (ding)
        (setq item-value (car vlist))
        (setq shu-keyring-last-key keyring-key)))
    (if item-value
        (kill-new item-value)
      (kill-new "unknown"))
    item-value
    ))


;;
;;  shu-keyring-show-name-url
;;
(defun shu-keyring-show-name-url (type item)
  "Show in the message area the name, url, or both of a keyring entry.  Also
prefix the message with the upper case type, which is the type of the entry that
has been placed in the clipboard, (PW, ID, etc.)"
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
      (setq mstring (shu-nvpindex-values-to-string names)))
     ((and urls (not names)) ;; urls only
      (setq mstring  (shu-nvpindex-values-to-string urls)))
     (t                       ;; Both names and urls
      (setq mstring (concat (shu-nvpindex-values-to-string urls) " (" (shu-nvpindex-values-to-string names) ")"))))
    (when got-string
      (setq mstring (concat mstring " ")))
    (when ids
      (setq mstring (concat mstring "id=" (shu-nvpindex-values-to-string ids))))
    (setq mstring (concat (upcase type) ": " mstring))
    (message "%s" mstring)
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
  "Put the external passphrase into the kill ring."
  (interactive)
  (let ((phrase "**unknown**")
        (displaypw ".........."))
    (if shu-keyring-external-passphrase
        (progn
          (setq phrase shu-keyring-external-passphrase)
          (message "Pass phrase: %s" displaypw))
      (ding)
      (message "%s" "Pass phrase is not set."))
    (shu-kill-new phrase)
    ))




;;
;;  shu-keyring-set-alternate-passphrase
;;
(defun shu-keyring-set-alternate-passphrase (phrase)
  "Function to read and set the alternate external pass phrase."
  (interactive "sPass phrase?: ")
  (setq shu-keyring-alternate-passphrase phrase)
  )


;;
;;  shu-keyring-get-alternate-passphrase
;;
(defun shu-keyring-get-alternate-passphrase ()
  "Put the alternate passphrase into the kill ring."
  (interactive)
  (let ((phrase "**unknown**")
        (displaypw ".........."))
    (if shu-keyring-alternate-passphrase
        (progn
          (setq phrase shu-keyring-alternate-passphrase)
          (message "Alternate pass phrase: %s" displaypw))
      (ding)
      (message "%s" "Alternate pass phrase is not set."))
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
  (defalias 'kroldpw 'shu-keyring-get-oldpw)
  (defalias 'krurl 'shu-keyring-get-url)
  (defalias 'krpin 'shu-keyring-get-pin)
  (defalias 'krid 'shu-keyring-get-id)
  (defalias 'kracct 'shu-keyring-get-acct)
  (defalias 'krrt 'shu-keyring-get-route)
  (defalias 'krclear 'shu-keyring-clear-last)
  (defalias 'krnewpw 'shu-keyring-generate-password)
  (defalias 'krfn 'shu-keyring-get-file)
  (defalias 'krvf 'shu-keyring-verify-file)
  (defalias 'krpps 'shu-keyring-get-passphrase)
  (defalias 'set-passphrase 'shu-keyring-set-passphrase)
  (defalias 'set-alternate-passphrase 'shu-keyring-set-alternate-passphrase)
  (defalias 'kraps 'shu-keyring-get-alternate-passphrase)
  )

;;; shu-keyring.el ends here
