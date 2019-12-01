;;; shu-new2.el --- Shu project code for dealing wth C++ in Emacs
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

;; A collection of experimental functions for dealing with C++ code.
;;
;;

;;; Code:


;;
;;  csp
;;
(defun csp (prefix)
  "Doc string."
  (interactive "*P")
  (let (
        (gb (get-buffer-create "**boo**"))
        (xquote "[^\\]\"") ;; quote not preceded by escape
        (tstart (shu-point-in-string))
        (tend)
        (p)
        (m)
        (cc)
        (pad-count 0)
        (bpad "")
        (npad "")
        (line-limit 10)
        (original)
        (fixed-width prefix)
        (lines)
        (line)
        (nl "")
        )
    ;; String to be removed is (tstart - 1) to tend (includes the quotes
    ;; String to be split and re-inserted is from tstart to (tend - 1 (Not
    ;; including the quotes
    (if (not tstart)
        (progn
          (ding)
          (message "%s" "Not in string")
          )
      (goto-char tstart)
      (setq cc (current-column))
      (when (> cc 0)
        (setq pad-count (1- cc))
        (setq bpad (make-string pad-count ? ))
        )
      (when (< pad-count shu-cpp-line-end)
        (setq line-limit (- shu-cpp-line-end pad-count 1))
        (when (< line-limit 10)
          (setq line-limit 10)
          )
        )
      (setq line-limit (1- line-limit))
      (princ (format "shu-cpp-line-end: %d, pad-count: %d, line-limit: %d\n" shu-cpp-line-end pad-count line-limit) gb)
      (setq tend (re-search-forward xquote nil t))
      (if (not tend)
          (message "%s" "No string end")
        (setq m (buffer-substring-no-properties tstart (1- tend)))
        (setq m (concat "[" m "]"))
        (message "%s" m)
        (setq original (buffer-substring-no-properties tstart (1- tend)))
        (princ (concat "[" original "]\n") gb)
        (setq lines (shu-misc-split-string original line-limit fixed-width))
        (goto-char (1- tstart))
        (delete-region (1- tstart) tend)
        (while lines
          (setq line (car lines))
          (insert (concat nl npad "\"" line "\""))
          (setq nl "\n")
          (setq npad bpad)
          (setq lines (cdr lines))
          )

        )
      )

    ))

                           ;;; "This is a string of some sort within these holy portals, revenge remains unknown and to all erring mortals, their way by love is shown and something else."


;;; shu-new2.el ends here
