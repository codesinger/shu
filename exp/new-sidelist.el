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
;;  shu-cpp-match-extract-info
;;
(defmacro shu-cpp-match-extract-info (match-info op-code match-eval-func
                                                 match-ret-ind match-token-type match-token-value)
  "Extract the information out of a match-info"
  (let ((tmatch-ext (make-symbol "match-ext"))
        (tmatch-func (make-symbol "match-func"))
        (tmatch-type (make-symbol "match-type")))
    `(let ((,tmatch-ext)
           (,tmatch-func)
           (,tmatch-type))
       (setq ,op-code (car ,match-info))
       (setq ,tmatch-ext (cdr ,match-info))
       (setq ,tmatch-func (car ,tmatch-ext))
       (setq ,tmatch-type (cdr ,tmatch-ext))
       (setq ,match-eval-func (cdr ,tmatch-func))
       (setq ,match-ret-ind (car ,tmatch-func))
       (setq ,match-token-type (car ,tmatch-type))
       (setq ,match-token-value (cdr ,tmatch-type)))
    ))


;;; new-sidelist.el ends here
