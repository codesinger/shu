;;; shu-org-extensions.el --- Shu project code for extending Emacs org mode
;;
;; Copyright (C) 2014 Stewart L. Palmer
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

;; The major function of this file is the function shu-org-archive-done-tasks,
;; which can be used as an after-save-hook for org files.  It finds each
;; TODO item that was marked DONE more than shu-org-archive-expiry days
;; ago and moves it to an archive file by invoking org-archive-subtree
;; on it.

(defvar shu-org-home "~/data/org"
  "Home directory of the org data files.")

(defvar shu-org-archive-expiry-days 7
  "Number of elapsed days before a closed TODO item is automatically archived.")

(defvar shu-org-todo-keywords (list "TODO" "WAITING")
  "Key words that represent the not DONE state.")

(defvar shu-org-done-keywords (list "DONE" "CANCELLED")
  "Key words that represent the DONE state.")


;;
;;  shu-org-done-projects-string
;;
(defun shu-org-done-projects-string()
  "Return a string that is a search for a TODO tag that does not contain any of the
words that represent a DONE item.  These are the words defined in org-done-keywords-for-agenda.
If the two keywords that mean finished item are DONE and CANCELLED, then this function will
return the string: TODO={.+}/-CANCELLED-DONE.  This is intended to be used in the definition
of the variable \"org-stuck-projects\"."
 (let
  ((qq    shu-org-done-keywords)
   (str   "TODO={.+}/")
   (t1    nil))

  (while qq
    (setq t1 (car qq))
    (setq str (concat str "-" t1))
    (setq qq (cdr qq)))
 str
))

;;
;;  shu-org-date-match-regexp
;;
(defun shu-org-date-match-regexp ()
  "Return a regexp string that matches an org date of the form 2012-04-01 Tue 13:18."
  (let (

    ;; Months 01 - 12
    (month-match (regexp-opt
        (list "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12") nil))

    ;; Day of month 01 - 31
    (day-match (regexp-opt
        (list "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" 
              "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" 
              "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31") nil ))

    ;; Must be twentieth or twenty-first centuray
    (year-start-match (regexp-opt
        (list "19" "20") nil ))

    ;; But may be any year in those centuries
    (year-end-match (regexp-opt
        (list "00" "01" "02" "03" "04" "05" "06" "07" "08" "09"
              "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
              "20" "21" "22" "23" "24" "25" "26" "27" "28" "29"
              "30" "31" "32" "33" "34" "35" "36" "37" "38" "39"
              "40" "41" "42" "43" "44" "45" "46" "47" "48" "49"
              "50" "51" "52" "53" "54" "55" "56" "57" "58" "59"
              "60" "61" "62" "63" "64" "65" "66" "67" "68" "69"
              "70" "71" "72" "73" "74" "75" "76" "77" "78" "79"
              "80" "81" "82" "83" "84" "85" "86" "87" "88" "89"
              "90" "91" "92" "93" "94" "95" "96" "97" "98" "99") nil ))

    ;; Must be one of these day names
    (day-name-match (regexp-opt
        (list " Mon " " Tue " " Wed " " Thu " " Fri " " Sat " " Sun ") nil ))

    ;; Twenty-four hour clock
    (hour-match (regexp-opt
        (list "00" "01" "02" "03" "04" "05" "06" "07" "08" "09"
              "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
              "20" "21" "22" "23" "24") nil ))

    ;; May be any minute within the hour
    (minute-match (regexp-opt
        (list "00" "01" "02" "03" "04" "05" "06" "07" "08" "09"
              "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
              "20" "21" "22" "23" "24" "25" "26" "27" "28" "29"
              "30" "31" "32" "33" "34" "35" "36" "37" "38" "39"
              "40" "41" "42" "43" "44" "45" "46" "47" "48" "49"
              "50" "51" "52" "53" "54" "55" "56" "57" "58" "59") nil ))

    (year-match )
    (date-match )
       )

  (setq year-match (concat year-start-match year-end-match))
  (setq date-match (concat
                         year-match  "-" 
                         month-match "-" 
                         day-match 
                         day-name-match
                         hour-match  ":"
                         minute-match))

  date-match
))

;;
;;  shu-org-state-regexp
;;
(defun shu-org-state-regexp(done-word)
  "Return a regular expression that will match a particular TODO state record of the form
   - State \"DONE\"       from \"CANCELLED\"  [2012-04-01 Tue 13:18]
  DONE-WORD is the desired state of the record."
  (let*
   ((date-match (concat "\\[\\(" (shu-org-date-match-regexp) "\\)\\]"))

    (todo-states (append (list "" ) shu-org-todo-keywords shu-org-done-keywords))
    (todo-state-regexp (regexp-opt todo-states))
    (done-match))

  (setq done-match (concat "\\s-*- State\\s-+" "\"" done-word "\"" "\\s-+" 
                           "from" "\\s-+" "\"" todo-state-regexp "\""
                           "\\s-+" "\\[" "\\(" (shu-org-date-match-regexp) "\\)" "\\]"))
  done-match
))

;;
;;  shu-org-archive-done-tasks
;;
;;  Original idea came from a similar function written by John Wiegley in 2007
;;
(defun shu-org-archive-done-tasks()
  "Go through an org file and archive any completed TODO item that was completed more
than shu-org-archive-expiry-days days ago."
  (interactive)
  (let
    ((ofile (concat shu-org-home "/auto-archive-log.txt"))
     (shu-debug nil) ;; Set to to dump debug information into "*shu debug*"
     (gbuf      nil)
     (debug-on-error t)
     (rex )
     (start-of-item )
     (end-of-item )
     (done-word )
     (when-done )
     (done-time-string )
     (done-days )
     (done-regexp
            (concat "\\* \\(" (regexp-opt shu-org-done-keywords) "\\) "))
     (todo-regexp 
            (concat "\\* " (regexp-opt (append shu-org-todo-keywords shu-org-done-keywords)) " "))
     (start-time (current-time))
     (cfile (buffer-file-name))
     (done-header )
     (header )
     (time-string )
     (age )
     (sbound )
     (archive-count 0)
     (error-count 0)
     (end-msg )
     (end-msg-a )
     (end-msg-b )
     (buffer-changed  nil))

(when (eq major-mode 'org-mode)
  (when shu-debug
     (setq  gbuf (get-buffer-create "*shu debug*")))
  (setq time-string (format-time-string "%Y-%m-%d %a %H:%M" start-time))
  (setq header (concat "\n\n******* Start TODO archive of " cfile 
                " on " time-string 
                          " *******\n\n"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; Look for a TODO item that is completed
      (while (re-search-forward done-regexp nil t)
        (when shu-debug
          (princ (concat "string " (match-string 1) "\n") gbuf)
          (princ (format "At line %d\n" (the-line-at (point))) gbuf))
        ;; The word that indicates DONE (something like DONE or CANCELLED)
        (setq done-word (match-string 1))
        (setq start-of-item (line-beginning-position))
        (setq end-of-item (line-end-position))
        ;; Build a regexp to search for the state transition record with a timestamp
        (setq rex (concat "- State\\s-+" "\"" done-word "\"" "\\s-+" 
                          "from" "\\s-+" "\"" "[A-Z]*" "\"" "\\s-+"
                           "\\[\\([0-9A-Za-z\-\:\s]+\\)\\]*"))
        (when shu-debug
          (princ (concat "rex = " rex "\n") gbuf))
        (setq rex (shu-org-state-regexp done-word))
        (setq sbound nil)
        (save-excursion
          (when (re-search-forward todo-regexp nil t)
            (setq sbound (line-beginning-position))))

        (when shu-debug
          (princ (format "sbound = %d (line %d)\n" sbound (the-line-at sbound)) gbuf))
        (if (re-search-forward rex sbound t)
          (progn
          (when shu-debug
            (princ (concat "Found " rex "\n") gbuf))
          (setq done-time-string (match-string 1))
          (setq end-of-item (line-end-position))
          (when shu-debug
            (princ (concat "Time: " done-time-string "\n") gbuf))
          (setq when-done (org-parse-time-string done-time-string))
          (setq done-days  (time-to-number-of-days
                             (time-subtract start-time
                              (apply #'encode-time when-done))))
          (when shu-debug
            (princ (format "Done days = %d\n" done-days) gbuf))
          (when  (>= done-days shu-org-archive-expiry-days)
            (when (not done-header)
              (append-to-file header nil ofile)
              (setq done-header t))

            (setq age (format "%d" done-days))
            (append-to-file (concat "Archiving (" age " days):\n") nil ofile)
            (append-to-file (concat (buffer-substring start-of-item end-of-item) "\n")
                                   nil ofile)
            (goto-char start-of-item)
            (org-archive-subtree)
            (setq archive-count (1+ archive-count))
            (setq buffer-changed t)))

            (when (not done-header)
              (append-to-file header nil ofile)
              (setq done-header t))

            (setq error-count (1+ error-count))
            (append-to-file (format "\n***error*** The following entry at line %d has no close time:\n" 
                            (the-line-at start-of-item)) nil ofile)
            (append-to-file (concat (buffer-substring start-of-item end-of-item) "\n\n")
                                   nil ofile)))

      (when buffer-changed
        (save-buffer)
        (when (or (> archive-count 0) (> error-count 0))
          (cond
            ((= error-count 0)
              (if (= archive-count 1)
                (setq end-msg (format "Archived %d completed item." archive-count))
                (setq end-msg (format "Archived %d completed items." archive-count))))
            ((= archive-count 0)
              (if (= error-count 1)
                (setq end-msg (format "Encountered %d error in attempting to archive completed items." error-count))
                (setq end-msg (format "Encountered %d errors in attempting to archive completed items." error-count))))
            (t 
              (if (= archive-count 1)
                (setq end-msg-a (format "Archived %d completed item " archive-count))
                (setq end-msg-a (format "Archived %d completed items " archive-count)))
              (if (= error-count 1)
                (setq end-msg-b (format "with %d error." error-count))
                (setq end-msg-b (format "with %d errors." error-count)))
              (setq end-msg (concat end-msg-a end-msg-b))))

          (message "%s" end-msg))))))
))
