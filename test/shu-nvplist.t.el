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

;;; Code


;;
;;  shu-test-shu-nvplist-parse
;;
(ert-deftest shu-test-shu-nvplist-parse1 ()
  (let ((item-list))
    (with-temp-buffer
      (insert
       (concat " < name=\"fred\" type = cat /> \n"
               " < a=b q=4 />"))
      (setq item-list (shu-nvplist-parse-buffer item-list))
      (shu-nvplist-show-item-list item-list))
    ))




;;
;;  shu-test-shu-nvplist-parse-buffer-1\
;;
(ert-deftest shu-test-shu-nvplist-parse-buffer-1\ ()
  (let ((gb (get-buffer-create "**boo**"))
        (data
         (concat
          "\n"
          " <id=mumble  try=fuumble car=\"Jaguar XKE\" />\n"
          " <cat=dog  bob=happy />\n"
          " <mary=lamb  stew=bubble fox=hen   />\n"
          ))
        (item-list)
        (item)
        (item-no)
        (items)
        (expected
         (list
          (cons 1
                (list (cons "id" "mumble") (cons "try" "fuumble") (cons "car" "Jaguar XKE")))
          (cons 2
                (list (cons "cat" "dog") (cons "bob" "happy")))
          (cons 3
                (list (cons "mary" "lamb") (cons "stew" "bubble") (cons "fox" "hen")))))
        (exp)
        (found)
        (xp)
        (x)
        (xx))
    (with-temp-buffer
      (insert data)
      (setq item-list (shu-nvplist-parse-buffer item-list))
      (shu-nvplist-show-item-list item-list)
      (while item-list
        (setq item (car item-list))
        (princ item gb)(princ "\n" gb)
        (should item)
        (should (consp item))
        (setq item-no (car item))
        (setq items (cdr item))
        (should item-no)
        (should (numberp item-no))
        (should (or (= 1 item-no) (= 2 item-no) (= 3 item-no)))
        (setq exp expected)
        (setq found nil)
        (while exp
          (setq xp (car exp))
          (setq x (car xp))
          (should x)
          (should (numberp x))
          (should (or (= 1 x) (= 2 x) (= 3 x)))
          (setq xx (cdr xp))
          (should xx)
          (should (listp xx))
          (when (= x item-no)
            (setq found t)
            (should (equal xx items)))
          (setq exp (cdr exp)))
        (setq item-list (cdr item-list))))
    ))


;;; shu-nvplist.t.el ends here
