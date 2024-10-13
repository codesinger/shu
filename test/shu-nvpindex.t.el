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
;;  shu-test-shu-nvpindex-make-primary-index-1
;;
(ert-deftest shu-test-shu-nvpindex-make-primary-index-1 ()
  (let ((buffer-name shu-unit-test-buffer)
        (item-list
         (list
          (cons 1
                (list (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
                      (cons "url" "www.fumble.com") (cons "car" "Jaguar XKE")))
          (cons 2
                (list (cons "name" "charlie") (cons "url" "www.google.com")
                      (cons "id" "dog") (cons "bob" "happy")))
          (cons 3
                (list (cons "name" "mary") (cons "url" "bubble") (cons "fox" "hen")
                      (cons "id" "123456")))))
        (keys
         (list
          "bob"
          "bubble"
          "charlie"
          "fred"
          "fumble.com"
          "google.com"
          "mary"
          "www.fumble.com"
          "www.google.com"))
        (index)
        (key-list)
        (key)
        (entry)
        (item))
    (setq index (shu-nvpindex-make-primary-index item-list buffer-name))
    (setq key-list keys)
    (while key-list
      (setq key (car key-list))
      (should key)
      (should (stringp key))
      (setq entry (assoc key index))
      (should entry)
      (should (consp entry))
      (setq item (cdr entry))
      (should item)
      (should (listp item))
      (should (listp item))
      (setq key-list (cdr key-list)))
    (princ index (get-buffer-create buffer-name))
    ))



;;
;;  shu-test-shu-nvpindex-make-primary-index-2
;;
(ert-deftest shu-test-shu-nvpindex-make-primary-index-2 ()
  (let ((buffer-name shu-unit-test-buffer)
        (gb (get-buffer-create "**noo**"))
        (item-list
         (list
          (cons 1
                (list (cons "name" "fred") (cons "name" "bob") (cons "id" "mumble")
                      (cons "url" "www.fumble.com") (cons "car" "Jaguar XKE")))
          (cons 2
                (list (cons "name" "charlie") (cons "url" "www.google.com")
                      (cons "id" "dog") (cons "bob" "happy")))
          (cons 3
                (list (cons "name" "mary") (cons "url" "bubble") (cons "fox" "hen")
                      (cons "id" "123456")))))
        (key-values
         (list
          (cons "bob" "mumble")
          (cons "bubble" "123456")
          (cons "charlie" "dog")
          (cons "fred" "mumble")
          (cons "fumble.com" "mumble")
          (cons "google.com" "dog")
          (cons "mary" "123456")
          (cons "www.fumble.com" "mumble")
          (cons "www.google.com" "dog")))
        (index)
        (kvs)
        (kv)
        (key)
        (value)
        (entry)
        (item)
        (vlist)
        (id)
        (debug-on-error t))
    (setq index (shu-nvpindex-make-primary-index item-list buffer-name))
    (setq kvs key-values)
    (while kvs
      (setq kv (car kvs))
      (setq key (car kv))
      (setq value (cdr kv))
      (should key)
      (should (stringp key))
      (should value)
      (should (stringp value))
      (princ (format "key: '%s', value: '%s'\n" key value) gb)
      (setq entry (assoc key index))
      (should entry)
      (should (consp entry))
      (setq item (cdr entry))
      (should item)
      (should (listp item))
      (princ "item: " gb)(princ item gb)(princ "\n" gb)
      (setq vlist (shu-nvplist-get-item-value "id" item))
      (princ "vlist: " gb)(princ vlist gb)(princ "\n" gb)
      (should vlist)
      (should (listp vlist))
      (should (= 1 (length vlist)))
      (setq id (car vlist))
      (should id)
      (should (stringp id))
      (should (string= id value))
      (setq kvs (cdr kvs)))
    (princ index (get-buffer-create buffer-name))
    ))


;;; shu-nvpindex.t.el ends here
