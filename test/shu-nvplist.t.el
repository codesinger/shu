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
;;  shu-test-shu-nvplist-parse-buffer-1
;;
(ert-deftest shu-test-shu-nvplist-parse-buffer-1 ()
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
        (princ "item: " gb)(princ item gb)(princ "\n" gb)
        (should item)
        (should (consp item))
        (setq item-no (car item))
        (setq items (cdr item))
        (princ "items: " gb)(princ items gb)(princ "\n" gb)
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
            (princ "xx: " gb)(princ xx gb)(princ "\n" gb)
            (should (equal xx items)))
          (setq exp (cdr exp)))
        (setq item-list (cdr item-list))))
    ))




;;
;;  shu-test-shu-nvplist-parse-buffer-2
;;
(ert-deftest shu-test-shu-nvplist-parse-buffer-2 ()
  (let ((gb (get-buffer-create "**boo**"))
        (data
         (concat
          "\n"
          " <id=mumble  car=Audi try=fuumble car=\"Jaguar XKE\" />\n"
          " <cat=dog  bob=happy />\n"
          " <fox=smile  mary=lamb  fox=eggs stew=bubble fox=hen   />\n"
          ))
        (item-list)
        (item)
        (item-no)
        (items)
        (expected
         (list
          (cons 1
                (list (cons "id" "mumble") (cons "car" "Audi") (cons "try" "fuumble") (cons "car" "Jaguar XKE")))
          (cons 2
                (list (cons "cat" "dog") (cons "bob" "happy")))
          (cons 3
                (list (cons "fox" "smile") (cons "mary" "lamb") (cons "fox" "eggs")
                      (cons "stew" "bubble") (cons "fox" "hen")))))
        (exp)
        (found)
        (xp)
        (x)
        (xx))
    (with-temp-buffer
      (princ "\n\n\n" gb)
      (insert data)
      (setq item-list (shu-nvplist-parse-buffer item-list))
      (shu-nvplist-show-item-list item-list)
      (while item-list
        (setq item (car item-list))
        (princ "item: " gb)(princ item gb)(princ "\n" gb)
        (should item)
        (should (consp item))
        (setq item-no (car item))
        (setq items (cdr item))
        (princ "items: " gb)(princ items gb)(princ "\n" gb)
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
            (princ "xx: " gb)(princ xx gb)(princ "\n" gb)
            (should (equal xx items)))
          (setq exp (cdr exp)))
        (setq item-list (cdr item-list))))
    ))




;;
;;  shu-test-shu-nvplist-get-names-1
;;
(ert-deftest shu-test-shu-nvplist-get-names-1 ()
  (let ((item
         (cons 88
         (list (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
               (cons "url" "www.fumble.com") (cons "uRl" "Jaguar XKE"))))
         (actual)
         (expected
          (list
           "id"
           "name"
           "url")))
    (setq actual (shu-nvplist-get-names item))
    (should actual)
    (should (listp actual))
    (should (equal actual expected))
    ))




;;
;;  shu-test-shu-nvplist-quoted-value-1
;;
(ert-deftest shu-test-shu-nvplist-quoted-value-1 ()
  (let ((value "Happy Birthday")
        (expected "\"Happy Birthday\"")
        (actual))
    (setq actual (shu-nvplist-quoted-value value))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-nvplist-quoted-value-2
;;
(ert-deftest shu-test-shu-nvplist-quoted-value-2 ()
  (let ((value "Happy=Birthday")
        (expected "\"Happy=Birthday\"")
        (actual))
    (setq actual (shu-nvplist-quoted-value value))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-nvplist-quoted-value-3
;;
(ert-deftest shu-test-shu-nvplist-quoted-value-3 ()
  (let ((value "Happy/Birthday")
        (expected "\"Happy/Birthday\"")
        (actual))
    (setq actual (shu-nvplist-quoted-value value))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-nvplist-quoted-value-4
;;
(ert-deftest shu-test-shu-nvplist-quoted-value-4 ()
  (let ((value "HappyBirthday")
        (expected "HappyBirthday")
        (actual))
    (setq actual (shu-nvplist-quoted-value value))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-nvplist-to-string-1
;;
(ert-deftest shu-test-shu-nvplist-to-string-1 ()
  (let ((data
         (list
          (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
          (cons "url" "www.fumble.com") (cons "car" "Jaguar XKE")))
        (expected "name=fred name=bob id=mumble url=www.fumble.com car=\"Jaguar XKE\"")
        (actual))
    (setq actual (shu-nvplist-to-string data))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-nvplist-to-string-2
;;
(ert-deftest shu-test-shu-nvplist-to-string-2 ()
  (let ((data
         (list
          (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
          (cons "url" "www.fumble.com") (cons "pw" "somesortofsecret") (cons "car" "Jaguar XKE")))
        (expected "name=fred name=bob id=mumble url=www.fumble.com pw=........ car=\"Jaguar XKE\"")
        (actual))
    (setq actual (shu-nvplist-to-string data))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-nvplist-item-to-string-1
;;
(ert-deftest shu-test-shu-nvplist-item-to-string-1 ()
  (let (
        (item
         (cons 88
         (list
          (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
          (cons "url" "www.fumble.com") (cons "car" "Jaguar XKE"))))
        (expected "      88: name=fred name=bob id=mumble url=www.fumble.com car=\"Jaguar XKE\"")
        (actual)
        )
    (setq actual (shu-nvplist-item-to-string item))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-nvplist-check-duplicates-1
;;
(ert-deftest shu-test-shu-nvplist-check-duplicates-1 ()
  (let ((buffer-name shu-unit-test-buffer)
        (item
         (cons 88
         (list (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
               (cons "url" "www.fumble.com") (cons "car" "Jaguar XKE"))))
        (key-list
         (list "account" "route" "id" "pw" "oldpw" "pin"))
        (dup-count))
    (setq dup-count (shu-nvplist-check-duplicates key-list item buffer-name))
    (should dup-count)
    (should (numberp dup-count))
    (should (= 0 dup-count))
    ))



;;
;;  shu-test-shu-nvplist-check-duplicates-2
;;
(ert-deftest shu-test-shu-nvplist-check-duplicates-2 ()
  (let ((buffer-name shu-unit-test-buffer)
        (item
         (cons 77
         (list (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
               (cons "id" "other") (cons "url" "www.fumble.com") (cons "pin" "1234")
               (cons "pin" "45678") (cons "car" "Jaguar XKE"))))
        (key-list
         (list "account" "route" "id" "pw" "oldpw" "pin"))
        (dup-count))
    (setq dup-count (shu-nvplist-check-duplicates key-list item buffer-name))
    (should dup-count)
    (should (numberp dup-count))
    (should (= 2 dup-count))
    ))



;;
;;  shu-test-shu-nvplist-check-duplicates-3
;;
(ert-deftest shu-test-shu-nvplist-check-duplicates-3 ()
  (let ((buffer-name shu-unit-test-buffer)
        (item
         (cons 105
         (list (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
               (cons "id" "other") (cons "url" "www.fumble.com") (cons "pin" "1234")
               (cons "pin" "45678") (cons "car" "Jaguar XKE")
               (cons "account" "Happy") (cons "route" "123456")
               (cons "oldpw" "old-one") (cons "pin" "hdhdhshdh")
               (cons "account" "Birthday") (cons "route" "123456")
               (cons "oldpw" "new-one"))))
        (key-list
         (list "account" "route" "id" "pw" "oldpw" "pin"))
        (dup-count))
    (setq dup-count (shu-nvplist-check-duplicates key-list item buffer-name))
    (should dup-count)
    (should (numberp dup-count))
    (should (= 5 dup-count))
    ))


;;; shu-nvplist.t.el ends here
