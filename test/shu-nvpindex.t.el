;;; shu-nvpindex.t.el --- Shu project code unit tests
;;
;; Copyright (C) 2024 Stewart L. Palmer
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
(require 'shu-nvpindex)

;;; Code


;;
;;  shu-test-shu-nvpindex-get-item-from-key-item
;;
(ert-deftest shu-test-shu-nvpindex-get-item-from-key-item ()
  (let ((index-entry (cons 3 4))
        (item))
    (setq item (shu-nvpindex-get-item-from-key-item index-entry))
    (should item)
    (should (numberp item))
    (should (= item 4))
    ))


;;
;;  shu-test-shu-nvpindex-get-key-from-key-item
;;
(ert-deftest shu-test-shu-nvpindex-get-key-from-key-item ()
  (let ((index-entry (cons 9 10))
        (item))
    (setq item (shu-nvpindex-get-key-from-key-item index-entry))
    (should item)
    (should (numberp item))
    (should (= item 9))
    ))



;;
;;  shu-test-shu-nvpindex-get-nvplist-from-item
;;
(ert-deftest shu-test-shu-nvpindex-get-nvplist-from-item ()
  (let ((item (cons 5 6))
        (nvplist))
    (setq nvplist (shu-nvpindex-get-nvplist-from-item item))
    (should nvplist)
    (should (numberp nvplist))
    (should (= nvplist 6))
    ))



;;
;;  shu-test-shu-nvpindex-get-item-number-from-item
;;
(ert-deftest shu-test-shu-nvpindex-get-item-number-from-item ()
  (let ((item (cons 7 8))
        (item-number))
    (setq item-number (shu-nvpindex-get-item-number-from-item item))
    (should item-number)
    (should (numberp item-number))
    (should (= item-number 7))
    ))


;;; shu-nvpindex.t.el ends here
