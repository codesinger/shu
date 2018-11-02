;;; shu-xref.t.el --- Shu project unit tests for code in shu-xref.el
;;
;; Copyright (C) 2015 Stewart L. Palmer
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

;;
;;  shu-xref.t.el
;;
;;  unit tests for code in shu-xref.el
;;


(require 'ert)
(require 'shu-xref)

;;
;;  shu-test-shu-xref-get-next-funcall
;;
(ert-deftest shu-test-shu-xref-get-next-funcall ()
  (let
    ((name "defun")
     (fun-name)
     (line-no))
  (with-temp-buffer (let ((a))
      (insert " (defun aaa ()")
      (goto-char (point-min))
      (setq a (shu-xref-get-next-funcall name a))
      (should a)
      (setq fun-name (car a))
      (setq line-no (cdr a))
      (should (string= "aaa" fun-name))
      (should (= 1 line-no))))

  (with-temp-buffer (let ((b))
      (insert "\n\n\n    (  defun bbb ()")
      (goto-char (point-min))
      (setq b (shu-xref-get-next-funcall name b))
      (should b)
      (setq fun-name (car b))
      (setq line-no (cdr b))
      (should (string= "bbb" fun-name))
      (should (= 4 line-no))))

  (with-temp-buffer (let ((c))
      (insert "\n\n\n\n    (  \n defun \n \t \f  ccc ()")
      (goto-char (point-min))
      (setq c (shu-xref-get-next-funcall name c))
      (should c)
      (setq fun-name (car c))
      (setq line-no (cdr c))
      (should (string= "ccc" fun-name))
      (should (= 5 line-no))))

  (with-temp-buffer (let ((d))
      (insert "   ; (defun ddd ()")
      (goto-char (point-min))
      (setq d (shu-xref-get-next-funcall name d))
      (should (not d))))

  (with-temp-buffer (let ((e))
      (insert "   \"Hello world.\"")
      (goto-char (point-min))
      (setq e (shu-xref-get-next-funcall name e))
      (should (not e))))

  (with-temp-buffer (let ((f))
      (insert "    (defun nnn ()  ;  ")
      (goto-char (point-min))
      (setq f (shu-xref-get-next-funcall name f))
      (should f)
      (setq fun-name (car f))
      (setq line-no (cdr f))
      (should (string= "nnn" fun-name))
      (should (= 1 line-no))))

  (with-temp-buffer (let ((g))
      (insert "\n\n\n\n ;   (  \n defun \n \t \f  eee ()")
      (goto-char (point-min))
      (setq g (shu-xref-get-next-funcall name g))
      (should (not g))))

  (with-temp-buffer (let ((h))
      (insert "\n\n\n\n    (  \n defun \n \t \f  a-beATle-name02_$ ()")
      (goto-char (point-min))
      (setq h (shu-xref-get-next-funcall name h))
      (should h)
      (setq fun-name (car h))
      (setq line-no (cdr h))
      (should (string= "a-beATle-name02_$" fun-name))
      (should (= 5 line-no))))

  (with-temp-buffer (let ((i) (j) (k))
      (insert "\n\n\n\n    (  \n defun \n \t \f  fff ()\n\n  (defun ggg ()")
      (goto-char (point-min))
      (setq i (shu-xref-get-next-funcall name i))
      (should i)
      (setq fun-name (car i))
      (setq line-no (cdr i))
      (should (string= "fff" fun-name))
      (should (= 5 line-no))
      (setq j (shu-xref-get-next-funcall name j))
      (should j)
      (setq fun-name (car j))
      (setq line-no (cdr j))
      (should (string= "ggg" fun-name))
      (should (= 9 line-no))
      (setq k (shu-xref-get-next-funcall name k))
      (should (not k))))
))
