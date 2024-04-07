;;; shu-bkmark.el --- Shu project code for dealing with remembered web sites
;;
;; Copyright (C) 2024 Stewart L. Palmer
;;
;; Package: shu-bkmark
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

;;
;; This is a set of functions for maintaining and querying a file of
;; remembered web sites.  Some web browsers support a bookmarks function for
;; remembering web sites of interest.  Different browsers have different ways
;; of storing and accessing their bookmarks.  The bookmarks are generally not
;; stored in a uniform manner.  If you use three different browsers, you have
;; three different ways to back up your bookmarks and new browser features
;; (such as Google Chrome profiles) can make your bookmarks mysteriously
;; vanish.
;;
;; This set of functions allows you to maintain a text file of remembered web
;; sites, to fetch them by a name you add or by the first few letters of the
;; URL, and to visit them in the browser of your choice.
;;
;; These functions have nothing to do with emacs bookmarks, which is a
;; completely different set of very useful functions.
;;
;; There is an emacs command that allows you to find a bookmark entry by name
;; or by the first few letters of the URL and to have the URL put into the
;; kill ring from whence you may paste it into your browser
;;
;; The bookmark file may be encrypted with GPG.  As of emacs 23, the EasyPG
;; package is included with the emacs distribution.  When you tell emacs to
;; open a file that has been encrypted with GPG, you are prompted for the
;; passphrase and the file is decrypted into memory.
;;
;; The file bookmarks.txt in the usr directory is am example of a small
;; bookmarks file that has not been encrypted.  Each entry in the file
;; consists of a set of name value pairs.  Each value may be enclosed in
;; quotes and must be enclosed in quotes if it contains embedded blanks.
;;
;; A single set of name value pairs starts with an opening "<" and is
;; terminated by a closing "/>".
;;
;; Here is an example of a set of two name value pairs:
;;
;;      < name=search url=www.google.com  />
;;
;; The names may be arbitrary but there are two names that are recognized by
;; the functions in this package.  They are:
;;
;; name represents the name of the entry.  This is the key that is used to
;; find the entry.  If no name is given, then the name of the URL is used.
;; If the URL starts with "www.", the "www." is removed to form the name.  An
;; entry that has no name and a URL of "www.facebook.com" would have an auto
;; generated name of "facebook.com".
;;
;; url represents a web address to be remembered.
;;
;; To use a bookmarks file, place the following lines in your .emacs file:
;;
;;      (load-file "/Users/fred/.emacs.d/shu-base.elc")
;;      (load-file "/Users/fred/.emacs.d/shu-nvplist.elc")
;;      (load-file "/Users/fred/.emacs.d/shu-nvpindex.elc")
;;      (load-file "/Users/fred/.emacs.d/shu-bkmark.elc")
;;      (shu-bkmark-set-alias)
;;      (setq shu-bkmark-file "/Users/fred/shu/usr/bookmarks.txt")
;;
;; replacing "/Users/fred/shu/usr/bookmarks.txt" with the path to your
;; bookmarks file.
;;
;; All of the shu functions require shu-base.
;;
;; If using the sample bookmarks file, You can now go to Walter Brown's talk
;; on "Modern Template Metaprogramming" as follows.
;;
;; Type M-x bkurl.  This prompts for the name of the desired key.  Type
;; "meta" and hit TAB to complete.  This fills out the name as
;; "metaprogramming" and puts the URL "www.youtube.com/watch?v=Am2is2QCvxY"
;; into the clip board.  Open a browser and paste the URL into it to go to
;; the video.
;;
;; The interactive commands defined in this file are as follows:
;;
;; kkurl   - Fetch the url for an entry
;;

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



(defgroup shu-bkmark nil
  "Maintain a file of remembered web sites."
  :group 'applications)

(defcustom shu-bkmark-file "~/bookmarks.txt"
  "Text file in which urls and names are stored."
  :type '(string)
  :group 'shu-bkmark)


(defvar shu-bkmark-index nil
  "The variable that points to the in-memory keyring index.")

(defvar shu-bkmark-history   nil
  "The history list used by completing-read when asking the user for a key to an
entry in the keyring file.")

(defconst shu-bkmark-url-name   "url"
  "Key word that denotes a URL.")

(defconst shu-bkmark-name-name   "name"
  "Key word that denotes a name.")

(defconst shu-bkmark-buffer-name  "**shu-bkmark**"
  "The name of the buffer into which keyring diagnostics and messages
are recorded.")

(defconst shu-bkmark-file-type "Bookmark"
  "The name of the file type of keyring file.  Used for diagnostic messages.")

;;
;; shu-bkmark-get-url
;;
(defun shu-bkmark-get-url()
  "Find the url for an entry in the keyring file.  This displays the entry in
the message area and puts the url into the kill ring so that it can be yanked
into a buffer or pasted into the application requesting it."
  (interactive)
  (shu-bkmark-get-field shu-bkmark-url-name)
  )


;;
;; shu-bkmark-get-file
;;
(defun shu-bkmark-get-file()
  "Display the name of the keyring file, if any.  This is useful if you are
getting unexpected results from some of the query functions that look up keyring
information.  Perhaps the unexpected results come from the fact that you are
using the wrong keyring file."
  (interactive)
  (if shu-bkmark-file
      (message "Shu keyring file is \"%s\"" shu-bkmark-file)
    (message "%s" "No keyring file is defined."))
  )

;;
;;  shu-bkmark-clear-index
;;
(defun shu-bkmark-clear-index()
  "This is called from after-save-hook to clear the keyring index if the keyring
file is saved.  The keyring index is built the first time it is needed and kept
in memory thereafter.  But we must refresh the index if the keyring file is
modified.  The easiest way to do this is to clear the index when the keyring
file is modified.  The next time the index is needed it will be recreated."
  (let
      ((fn1 (buffer-file-name))
       (fn2 (abbreviate-file-name (buffer-file-name))))

    (when (or (string= shu-bkmark-file fn1)
              (string= shu-bkmark-file fn2))
      (setq shu-bkmark-index nil))
    ))


;;
;;  shu-bkmark-verify-file
;;
(defun shu-bkmark-verify-file ()
  "Parse and verify the keyring file, displaying the result of the operation in the
keyring buffer (**shu-bkmark**).  If one of the queries for a url or other
piece of information is unable to find the requested information, it could be
that you have the wrong keyring file or that there is a syntax error in the
keyring file.  shu-bkmark-get-file (alias krfn) displays the name of the
keyring file.  This function parses the keyring file.  After the operation. look
into the keyring buffer (**shu-bkmark**) to see if there are any complaints
about syntax errors in the file."
  (interactive)
  (setq shu-bkmark-index nil)
  (when (bufferp shu-bkmark-buffer-name)
    (kill-buffer shu-bkmark-buffer-name))
  (setq shu-bkmark-index
        (shu-nvpindex-parse-file shu-bkmark-file shu-bkmark-file-type shu-bkmark-buffer-name))
  (switch-to-buffer shu-bkmark-buffer-name)
  (goto-char (point-min))
  )



;;
;;  shu-bkmark-get-field
;;
(defun shu-bkmark-get-field (name)
  "Fetch the value of a named field from the keyring.  Prompt the user with a
completing-read for the field that identifies the key.  Use the key to find the
item.  Find the value of the named key value pair within the item.  Put the
value in the kill-ring and also return it to the caller."
  (let ((gbuf      (get-buffer-create shu-bkmark-buffer-name))
        (invitation   "Key? ")
        (keyring-key   )
        (keyring-entry )
        (item          )
        (vlist         )
        (item-value    ))
    (when (not shu-bkmark-index)
      (setq shu-bkmark-index
            (shu-nvpindex-parse-file shu-bkmark-file shu-bkmark-file-type shu-bkmark-buffer-name)))
    (if (not shu-bkmark-index)
        (progn
          (message "Could not parse keyring.  See %s." shu-bkmark-buffer-name)
          (ding))

      (let
          ((completion-ignore-case t))
        (setq keyring-key
              (completing-read
               invitation  ;; prompt
               shu-bkmark-index   ;; collection
               nil         ;; predicate function
               nil         ;; require-match
               nil         ;; Initial input (initial minibuffer contents)
               shu-bkmark-history  ;; History list
               nil)))      ;; Default value
      (setq keyring-entry  (assoc keyring-key shu-bkmark-index))
      (setq item (cdr keyring-entry))
      (shu-bkmark-show-name-url name item)
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
;;  shu-bkmark-show-name-url
;;
(defun shu-bkmark-show-name-url (type item)
  "Show in the message area the name, url, or both of a keyring entry.  Also
prefix the message with the upper case type, which is the type of the entry that
has been placed in the clipboard, (PW, ID, etc.)"
  (let ((names   (shu-nvplist-get-item-value shu-bkmark-name-name item))
        (urls    (shu-nvplist-get-item-value shu-bkmark-url-name item))
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
    (setq mstring (concat (upcase type) ": " mstring))
    (message "%s" mstring)
    ))


;;
;;  shu-bkmark-set-alias
;;
(defun shu-bkmark-set-alias ()
  "Set the common alias names for the functions in shu-bkmark.
These are generally the same as the function names with the leading
shu- prefix removed.  But in this case the names are drastically shortened
to make them easier to type. "
  (defalias 'bkurl 'shu-bkmark-get-url)
  (defalias 'bkfn 'shu-bkmark-get-file)
  (defalias 'bkvf 'shu-bkmark-verify-file)
  )

;;; shu-bkmark.el ends here
