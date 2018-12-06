;;; shu-nvplist.t.el --- Shu project code unit tests
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Author: Stewart L. Pslmer <stewart@stewartpalmer.com>
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
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

(require 'ert)
(require 'shu-nvplist)



;;
;;  shu-test-shu-csplit
;;
(ert-deftest shu-test-shu-nvplist-parse1 ()
  (let (
        (item-list)
        )
    (with-temp-buffer
      (insert
       (concat " < name=\"fred\" type = cat /> \n"
               " < a=b q=4 />"))
      (setq item-list (shu-nvplist-parse-buffer item-list))
      (shu-nvplist-show-item-list item-list))
    ))
