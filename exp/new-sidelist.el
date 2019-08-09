;;; new-sidelist.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-match
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

;; Experimental code for dealing with side-lists of match-lists
;;

;;; Code:

(provide 'shu-cpp-match)
(require 'shu-cpp-token)


;;
;;
;; If op-code indicates that this is a side list, then we have the structure
;; shown below, with side-list pointing to the first match-info in the side
;; list.
;;
;;
;;  match-info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-side
;;        |
;;        +-------------> op-code
;;
;;
;;
;;  match-side
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> side-list
;;        |
;;        +-------------> side-parameter
;;
;;
;;  side-parameter is whatever the side list requires
;;



;;
;;  shu-cpp-make-match-side-list
;;
(defun shu-cpp-make-match-side-list (op-code match-list &optional side-parameter)
  "Return a match-info structure from the given arguments that represents a
side list."
  (let (
        (match-side (cons side-parameter match-list))
        )
  (cons op-code match-side)
  ))



;;
;;  shu-cpp-match-extract-side-list
;;
(defmacro shu-cpp-match-extract-side-list (match-info op-code side-list side-parameter)
  "Extract the side-list information out of a match-info that represents a side-list.."
  (let ((tmatch-side (make-symbol "match-side")))
    `(let ((,tmatch-side))
       (setq ,op-code (car ,match-info))
       (setq ,tmatch-side (cdr ,match-info))
       (setq ,side-parameter (car ,tmatch-side))
       (setq ,side-list (cdr ,tmatch-side)))
    ))



;;
;;  shu-cpp-match-extract-side-list-only
;;
(defun shu-cpp-match-extract-side-list-only (match-info)
  "Extract only the side list from the match info.  This is in contract to
shu-cpp-match-extract-side-list, which extracts all of the properties of a
side list."
    (cddr match-info)
    )






;;
;;  shu-test-shu-cpp-make-match-side-list-1
;;
(ert-deftest shu-test-shu-cpp-make-match-side-list-1 ()
  (let (
        (op-code 101)
        (match-list 303)
        (side-parameter)
        (match-info)
        (xop-code)
        (xmatch-list)
        (xside-parameter)
        )
    (setq match-info (shu-cpp-make-match-side-list op-code match-list))
    (shu-cpp-match-extract-side-list match-info xop-code xmatch-list xside-parameter)
    (should (= op-code xop-code))
    (should (= match-list xmatch-list))
    (should (equal side-parameter xside-parameter))

    (setq op-code 202)
    (setq match-list 303)
    (setq side-parameter 404)
    (setq match-info (shu-cpp-make-match-side-list op-code match-list side-parameter))
    (shu-cpp-match-extract-side-list match-info xop-code xmatch-list xside-parameter)
    (should (= op-code xop-code))
    (should (= match-list xmatch-list))
    (should (= side-parameter xside-parameter))
    ))



;;; new-sidelist.el ends here
