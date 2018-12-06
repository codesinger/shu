;;; shu-base.t.el --- Shu project code unit tests
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

(defvar shu-test-point-list
  "List of points to go to")
(defvar shu-test-line-list
  "List of corresponding line numbers for above")
(defvar shu-test-start-narrow
  "Narrowed region start")
(defvar shu-test-end-narrow
  "Narrowed region end")


;;
;;  shu-test-fillbuffer-ipsum
;;
(defun shu-test-fillbuffer-ipsum ()
  "Fill the current buffer with 59 lines of lorem ipsum text on which test cases may
operate.  The following variables must be declared by the caller, which will then be
bound to this function.  SHU-TEST-POINT-LIST is a returned list of points.
SHU-TEST-LINE-LIST is a returned list of line numbers corresponding to the points in
SHU-TEST-POINT-LIST.  SHU-TEST-START-NARROW and SHU-TEST-END-NARROW are two points
in the buffer that may be used to narrow it in such a way that at least some of the
points in SHU-TEST-POINT-LIST fall outside of the narrowed region."
  (let ((x))
    (insert
     (concat
      " 1:Lorem ipsum dolor sit amet, consectetur adipiscing\n"
      " 2:elit. Vestibulum at mauris venenatis, dignissim lacus\n"
      " 3:mattis, venenatis nisl. Proin ac vestibulum ante, eget\n"
      " 4:maximus turpis. Fusce varius massa eget lobortis\n"
      " 5:pulvinar. Pellentesque dictum dui sit amet mi\n"
      " 6:pellentesque, id vulputate nisi laoreet. Maecenas\n"
      " 7:interdum lectus ipsum. Fusce lacus nisl, sollicitudin\n"
      " 8:ac fermentum quis, eleifend vitae enim. Aenean auctor\n"
      " 9:quam sit amet risus vulputate, et imperdiet risus\n"))
    (setq x (1- (point)))
    (setq shu-test-point-list (cons x shu-test-point-list))
    (setq shu-test-line-list (cons 9 shu-test-line-list))
    (insert
     (concat
      "10:lacinia. Morbi at lobortis est, vitae fermentum quam.\n"
      "\n"
      "12:Fusce volutpat nec nulla sed faucibus. Sed nec ipsum\n"
      "13:lacus. Aliquam maximus eleifend pulvinar. Vestibulum\n"
      "14:interdum dolor enim, sit amet scelerisque magna\n"
      "15:consectetur eu. Suspendisse scelerisque diam eu odio\n"
      "16:tristique, nec auctor tellus lobortis. In tempus dolor\n"
      "17:nec tortor condimentum, ut posuere nisi\n"
      "18:finibus. Integer libero lorem, malesuada at elit sit\n"))
    (setq x (1- (point)))
    (setq shu-test-start-narrow (1- x))
    (setq shu-test-point-list (cons x shu-test-point-list))
    (setq shu-test-line-list (cons 18 shu-test-line-list))
    (insert
     (concat
      "19:amet, interdum tempor nisl. Nullam arcu sapien,\n"
      "20:tincidunt a bibendum sit amet, ornare vel risus. Cras\n"
      "21:suscipit nisi in justo sollicitudin\n"
      "22:commodo. Suspendisse at dolor mauris. Aenean consequat\n"
      "23:tortor a varius maximus. Vivamus eleifend elementum\n"
      "24:erat, dictum ultrices nisi maximus vel.\n"))
    "25:\n"
    (setq x (point))
    (setq shu-test-end-narrow (1+ x))
    (setq shu-test-point-list (cons x shu-test-point-list))
    (setq shu-test-line-list (cons 25 shu-test-line-list))
    (insert
     (concat
      "26:Duis sapien nunc, vehicula et mauris in, sagittis\n"
      "27:finibus ipsum. Interdum et malesuada fames ac ante\n"
      "28:ipsum primis in faucibus. Cras gravida mauris nec\n"
      "29:mauris tincidunt, elementum iaculis dolor\n"
      "30:vulputate. Aenean nibh leo, auctor ut dui sit amet,\n"
      "31:eleifend tempor ipsum. Quisque semper purus ac\n"
      "32:pellentesque auctor. Sed vitae imperdiet magna. Ut\n"
      "33:gravida varius semper. Praesent at odio tempor, feugiat\n"
      "34:quam vel, rutrum libero.\n"
      "35:\n"
      "36:Duis eget consequat lectus, eget efficitur\n"
      "37:neque. Aliquam risus ligula, ultrices vel nisl cursus,\n"
      "38:bibendum viverra neque. Morbi laoreet porttitor\n"
      "39:sollicitudin. Vivamus vitae tincidunt elit, quis\n"
      "40:sollicitudin. Vivamus vitae tincidunt elit, quis\n"
      "41:consectetur velit. Integer elementum ut nibh et\n"))
    (setq x (point))
    (setq shu-test-point-list (cons x shu-test-point-list))
    (setq shu-test-line-list (cons 41 shu-test-line-list))
    (insert
     (concat
      "42:tempor. Morbi vel sapien sed tortor commodo volutpat et\n"
      "43:sit amet mi. Suspendisse potenti. Ut justo diam, porta\n"
      "44:suscipit leo eget, malesuada sagittis elit. Suspendisse\n"
      "45:sed efficitur lorem. Mauris quis dolor ut leo maximus\n"
      "46:sodales non sed odio. Suspendisse potenti. Suspendisse\n"
      "47:sed aliquam urna, sit amet varius quam.\n"
      "48:\n"
      "49:Sed ut interdum turpis, id sollicitudin ipsum. Duis non\n"
      "50:sodales ante, in malesuada lectus. Praesent eu blandit\n"))
    ))

;;
;;  shu-test-shu-the-line-at
;;
(ert-deftest shu-test-shu-the-line-at ()
  (let ((x)             ;; current value of (point)
       (y)             ;; Current line number
       (shu-test-point-list)    ;; List of points to go to
       (shu-test-line-list)     ;; List of corresponding line numbers for above
       (shu-test-start-narrow)  ;; Narrowed region start
       (shu-test-end-narrow))   ;; Narrowed region end

    (with-temp-buffer
      (shu-test-fillbuffer-ipsum)
      (goto-char (point-min))
      (should (= 1 (shu-the-line-at (point-min))))
      (shu-goto-line 5)
      (should (= 5 (shu-the-line-at (point))))
      (let ((i) (v))
        (dotimes (i 48 v)
          (shu-goto-line (1+ i))
          (should (= (1+ i) (shu-the-line-at (point))))))

      (save-excursion
        (narrow-to-region shu-test-start-narrow shu-test-end-narrow)
        (while shu-test-point-list
          (setq x (car shu-test-point-list))
          (setq y (car shu-test-line-list))
          (should (= y (shu-the-line-at x)))
          (setq shu-test-point-list (cdr shu-test-point-list))
          (setq shu-test-line-list  (cdr shu-test-line-list))))
      )))


;;
;;  shu-test-shu-current-line
;;
(ert-deftest shu-test-shu-current-line ()
  (let ((x)             ;; current value of (point)
       (y)             ;; Current line number
       (shu-test-point-list)    ;; List of points to go to
       (shu-test-line-list)     ;; List of corresponding line numbers for above
       (shu-test-start-narrow)  ;; Narrowed region start
       (shu-test-end-narrow))   ;; Narrowed region end

    (with-temp-buffer
      (shu-test-fillbuffer-ipsum)
      (goto-char (point-min))
      (should (= 1 (shu-current-line)))
      (shu-goto-line 5)
      (should (= 5 (shu-current-line)))
      (let ((i) (v))
        (dotimes (i 48 v)
          (shu-goto-line (1+ i))
          (should (= (1+ i) (shu-current-line)))))

      (while shu-test-point-list
        (setq x (car shu-test-point-list))
        (setq y (car shu-test-line-list))
        (save-restriction (widen) (goto-char x))
        (should (= y (shu-current-line)))
        (setq shu-test-point-list (cdr shu-test-point-list))
        (setq shu-test-line-list  (cdr shu-test-line-list)))
      )))



;;
;;  shu-test-shu-point-in-string
;;
(ert-deftest shu-test-shu-point-in-string ()
  (let ((gbuf (get-buffer-create shu-unit-test-buffer))
       (p1))
    (with-temp-buffer
      (insert "\"Hello\"")
      (forward-char -4)
      (setq p1 (shu-point-in-string))
      (should (= 2 p1)))

    (with-temp-buffer
      (insert " \"Hello\"")
      (forward-char -4)
      (setq p1 (shu-point-in-string))
      (should (= 3 p1)))

    (with-temp-buffer
      (insert " \"Hello\"")
      (setq p1 (shu-point-in-string))
      (should (not p1)))

    (with-temp-buffer
      (insert " \"o\"")
      (forward-char -2)
      (setq p1 (shu-point-in-string))
      (should (= 3 p1)))

    (with-temp-buffer
      (insert " \"\"")
      (forward-char -1)
      (setq p1 (shu-point-in-string))
      (should (= 3 p1)))

    (with-temp-buffer
      (insert " \"Hello\"\n\n\n        \"Hello\"")
      (forward-char -9)
      (setq p1 (shu-point-in-string))
      (should (not p1)))
    ))


;;
;;  shu-test-shu-remove-trailing-whitespace-1
;;
(ert-deftest shu-test-shu-remove-trailing-all-whitespace-1 ()
  (let ((s1 "Hello    ")
       (ts1 "Hello")
       (result))
    (setq result (shu-remove-trailing-all-whitespace s1))
    (should (string= ts1 result))))


;;
;;  shu-test-shu-remove-trailing-whitespace-2
;;
(ert-deftest shu-test-shu-remove-trailing-all-whitespace-2 ()
  (let ((s1 "   Hello    \n")
       (ts1 "   Hello")
       (result))
    (setq result (shu-remove-trailing-all-whitespace s1))
    (should (string= ts1 result))))


;;
;;  shu-test-shu-remove-trailing-whitespace-3
;;
(ert-deftest shu-test-shu-remove-trailing-all-whitespace-3 ()
  (let ((s1 "   Hello     \b \t \n \v \f \r")
       (ts1 "   Hello")
       (result))
    (setq result (shu-remove-trailing-all-whitespace s1))
    (should (string= ts1 result))))


;;
;;  shu-test-shu-format-num-1
;;
(ert-deftest shu-test-shu-format-num-1 ()
  (let ((x 902)
      (gx "  902")
       (result))
    (setq result (shu-format-num x 5))
    (should (string= gx result))))


;;
;;  shu-test-shu-format-num-2
;;
(ert-deftest shu-test-shu-format-num-2 ()
  (let ((x 902)
      (gx "902")
       (result))
    (setq result (shu-format-num x 2))
    (should (string= gx result))))


;;
;;  shu-test-shu-fixed-format-num-1
;;
(ert-deftest shu-test-shu-fixed-format-num-1 ()
  (let ((x 902)
      (gx "  902")
       (result))
    (setq result (shu-fixed-format-num x 5))
    (should (string= gx result))))


;;
;;  shu-test-shu-fixed-format-num-2
;;
(ert-deftest shu-test-shu-fixed-format-num-2 ()
  (let ((x 902)
      (gx "902")
       (result))
    (setq result (shu-fixed-format-num x 2))
    (should (string= gx result))))


;;
;;  shu-test-shu-fixed-format-num-3
;;
(ert-deftest shu-test-shu-fixed-format-num-3 ()
  (let ((x 103902)
      (gx "  103,902")
       (result))
    (setq result (shu-fixed-format-num x 9))
    (should (string= gx result))))


;;
;;  shu-test-shu-group-number-1
;;
(ert-deftest shu-test-shu-group-number-1 ()
  (let ((x 902)
      (gx "902")
       (result))
    (setq result (shu-group-number x))
    (should (string= gx result))))


;;
;;  shu-test-shu-group-number-2
;;
(ert-deftest shu-test-shu-group-number-2 ()
  (let ((x "902")
      (gx "902")
       (result))
    (setq result (shu-group-number x))
    (should (string= gx result))))


;;
;;  shu-test-shu-group-number-3
;;
(ert-deftest shu-test-shu-group-number-3 ()
  (let ((x 1902)
      (gx "1,902")
       (result))
    (setq result (shu-group-number x))
    (should (string= gx result))))


;;
;;  shu-test-shu-group-number-4
;;
(ert-deftest shu-test-shu-group-number-4 ()
  (let ((x 1101902)
      (gx "1,101,902")
       (result))
    (setq result (shu-group-number x))
    (should (string= gx result))))


;;
;;  shu-test-shu-group-number-5
;;
(ert-deftest shu-test-shu-group-number-5 ()
  (let ((x 1101902)
      (gx "1;10;19;02")
       (result))
    (setq result (shu-group-number x 2 ";"))
    (should (string= gx result))))
