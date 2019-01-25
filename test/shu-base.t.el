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

;;; Code


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
;;  shu-test-shu-point-in-string-1
;;
(ert-deftest shu-test-shu-point-in-string-1 ()
  (let ((gbuf (get-buffer-create shu-unit-test-buffer))
       (p1))
    (with-temp-buffer
      (insert "\"Hello\"")
      (forward-char -4)
      (setq p1 (shu-point-in-string))
      (should (= 2 p1)))

    (with-temp-buffer
      (insert "    \"Hello\"")
      (forward-char -4)
      (setq p1 (shu-point-in-string))
      (should (= 6 p1)))

    (with-temp-buffer
      (insert " \"Hello\"")
      (setq p1 (shu-point-in-string))
      (should (not p1)))

    (with-temp-buffer
      (insert  "         \"o\"")
      (forward-char -2)
      (setq p1 (shu-point-in-string))
      (should (= 11 p1)))

    (with-temp-buffer
      (insert " \"\"")
      (forward-char -1)
      (setq p1 (shu-point-in-string))
      (should (not p1)))

    (with-temp-buffer
      (insert " \"Hello\"\n\n\n        \"Hello\"")
      (forward-char -9)
      (setq p1 (shu-point-in-string))
      (should (not p1)))
    ))



;;
;;  shu-test-shu-point-in-string-2
;;
(ert-deftest shu-test-shu-point-in-string-2 ()
  (let ((gbuf (get-buffer-create shu-unit-test-buffer))
       (p1))
    (with-temp-buffer
      (insert "\"A\"\"B\"")
      (goto-char 5)
      (setq p1 (shu-point-in-string))
      (should (= 5 p1)))
    (with-temp-buffer
      (insert "\"A\"")
      (goto-char 2)
      (setq p1 (shu-point-in-string))
      (should (= 2 p1)))
    (with-temp-buffer
      (insert " \"A\"")
      (goto-char 3)
      (setq p1 (shu-point-in-string))
      (should (= 3 p1)))
    (with-temp-buffer
      (insert "  \"A\"")
      (goto-char 4)
      (setq p1 (shu-point-in-string))
      (should (= 4 p1)))
    (with-temp-buffer
      (insert "   \"A\"")
      (goto-char 5)
      (setq p1 (shu-point-in-string))
      (should (= 5 p1)))
))



;;
;;  shu-test-shu-point-in-string-3
;;
(ert-deftest shu-test-shu-point-in-string-3 ()
  (let ((gbuf (get-buffer-create shu-unit-test-buffer))
       (p1))
    (with-temp-buffer
      (insert "\"A\"\"B\"")
      (goto-char 6)
      (setq p1 (shu-point-in-string))
      (should (not p1)))
))



;;
;;  shu-test-shu-point-in-string-4
;;
(ert-deftest shu-test-shu-point-in-string-4 ()
  (let ((gbuf (get-buffer-create shu-unit-test-buffer))
       (p1))
    (with-temp-buffer
      (insert "\"A\"\"B\"")
      (setq p1 (shu-point-in-string 5))
      (should (= 5 p1)))
    (with-temp-buffer
      (insert "\"A\"")
      (setq p1 (shu-point-in-string 2))
      (should (= 2 p1)))
    (with-temp-buffer
      (insert " \"A\"")
      (setq p1 (shu-point-in-string 3))
      (should (= 3 p1)))
    (with-temp-buffer
      (insert "  \"A\"")
      (setq p1 (shu-point-in-string 4))
      (should (= 4 p1)))
    (with-temp-buffer
      (insert "   \"A\"")
      (setq p1 (shu-point-in-string 5))
      (should (= 5 p1)))
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
;;  shu-test-shu-format-num-3
;;
(ert-deftest shu-test-shu-format-num-3 ()
  (let ((x 902)
      (gx "00902")
       (result))
    (setq result (shu-format-num x 5 ?0))
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



;;
;;  shu-test-shu-remove-trailing-all-whitespace-1
;;
(ert-deftest shu-test-shu-remove-trailing-all-whitespace-1 ()
  (let* ((string  " \t Now is the time for all good men ...")
         (expected string)
         (actual))
    (setq actual (shu-remove-trailing-all-whitespace string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-remove-trailing-all-whitespace-2
;;
(ert-deftest shu-test-shu-remove-trailing-all-whitespace-2 ()
  (let* ((data "  Now is the time for all good men ...")
         (string  (concat data " \t "))
         (expected data)
         (actual))
    (setq actual (shu-remove-trailing-all-whitespace string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-remove-trailing-all-whitespace-3
;;
(ert-deftest shu-test-shu-remove-trailing-all-whitespace-3 ()
  (let* ((data "  Now is the time for all good men ...")
         (string  (concat data "    "))
         (expected data)
         (actual))
    (setq actual (shu-remove-trailing-all-whitespace string))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-trim-leading-1
;;
(ert-deftest shu-test-shu-trim-leading-1 ()
  (let* ((string  "Now is the time for all good men ...   ")
         (expected string)
         (actual))
    (setq actual (shu-trim-leading string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-leading-2
;;
(ert-deftest shu-test-shu-trim-leading-2 ()
  (let* ((data "Now is the time for all good men ...    ")
         (string  (concat " \t " data))
         (expected data)
         (actual))
    (setq actual (shu-trim-leading string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-leading-3
;;
(ert-deftest shu-test-shu-trim-leading-3 ()
  (let* ((data "Now is the time for all good men ...    ")
         (string  (concat "    " data))
         (expected data)
         (actual))
    (setq actual (shu-trim-leading string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-trailing-1
;;
(ert-deftest shu-test-shu-trim-trailing-1 ()
  (let* ((string  " \t Now is the time for all good men ...")
         (expected string)
         (actual))
    (setq actual (shu-trim-trailing string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-trailing-2
;;
(ert-deftest shu-test-shu-trim-trailing-2 ()
  (let* ((data "  Now is the time for all good men ...")
         (string  (concat data " \t "))
         (expected data)
         (actual))
    (setq actual (shu-trim-trailing string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-trailing-3
;;
(ert-deftest shu-test-shu-trim-trailing-3 ()
  (let* ((data "  Now is the time for all good men ...")
         (string  (concat data "    "))
         (expected data)
         (actual))
    (setq actual (shu-trim-trailing string))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-trim-1
;;
(ert-deftest shu-test-shu-trim-1 ()
  (let* ((string  "Now is the time for all good men ...")
         (expected string)
         (actual))
    (setq actual (shu-trim-trailing string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-2
;;
(ert-deftest shu-test-shu-trim-2 ()
  (let* ((data "Now is the time for all good men ...")
         (string  (concat " \t " data))
         (expected data)
         (actual))
    (setq actual (shu-trim string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-3
;;
(ert-deftest shu-test-shu-trim-3 ()
  (let* ((data "Now is the time for all good men ...")
         (string  (concat data " \t "))
         (expected data)
         (actual))
    (setq actual (shu-trim string))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-4
;;
(ert-deftest shu-test-shu-trim-4 ()
  (let* ((data "Now is the time for all good men ...")
         (string  (concat " \t " data " \t "))
         (expected data)
         (actual))
    (setq actual (shu-trim string))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-minimum-leading-space-1
;;
(ert-deftest shu-test-shu-minimum-leading-space-1 ()
  (let* ((data "    Now is the time ...")
         (expected 2)
         (actual)
         (pos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq pos (point))
      (setq actual (shu-minimum-leading-space expected))
      (should (= expected actual))
      (should (= pos (point))))
    ))




;;
;;  shu-test-shu-minimum-leading-space-2
;;
(ert-deftest shu-test-shu-minimum-leading-space-2 ()
  (let* ((data "    Now is the time ...")
         (expected 4)
         (want 8)
         (actual)
         (pos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq pos (point))
      (setq actual (shu-minimum-leading-space want))
      (should (= expected actual))
      (should (= pos (point))))
    ))




;;
;;  shu-test-shu-minimum-leading-space-3
;;
(ert-deftest shu-test-shu-minimum-leading-space-3 ()
  (let* ((data " \t  Now is the time ...")
         (expected 4)
         (want 4)
         (actual)
         (pos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq pos (point))
      (setq actual (shu-minimum-leading-space want))
      (should (= expected actual))
      (should (= pos (point))))
    ))


;;
;;  shu-test-shu-end-of-string-1
;;
(ert-deftest shu-test-shu-end-of-string-1 ()
  (let ((p1)
        (expected 5)
        (pos))
    (with-temp-buffer
      (insert "\"As\"")
      (goto-char 2)
      (setq pos (point))
      (setq p1 (shu-end-of-string "\""))
      (should (= expected (point)))
      (should (= expected p1)))
    ))


;;
;;  shu-test-shu-end-of-string-2
;;
(ert-deftest shu-test-shu-end-of-string-2 ()
  (let ((p1)
        (expected 5)
        (pos))
    (with-temp-buffer
      (insert "'As'")
      (goto-char 2)
      (setq pos (point))
      (setq p1 (shu-end-of-string "'"))
      (should (= expected (point)))
      (should (= expected p1)))
    ))


;;
;;  shu-test-shu-end-of-string-3
;;
(ert-deftest shu-test-shu-end-of-string-3 ()
  (let ((p1)
        (expected 12)
        (pos))
    (with-temp-buffer
      (insert "'Something'")
      (goto-char 2)
      (setq pos (point))
      (setq p1 (shu-end-of-string "'"))
      (should (= expected (point)))
      (should (= expected p1)))
    ))


;;
;;  shu-test-shu-end-of-string-4
;;
(ert-deftest shu-test-shu-end-of-string-4 ()
  (let ((p1)
        (pos))
    (with-temp-buffer
      (insert "'Something")
      (goto-char 2)
      (setq pos (point))
      (setq p1 (shu-end-of-string "'"))
      (should (= 3 (point)))
      (should (not p1)))
    ))



;;
;;  shu-test-shu-add-to-alist-1
;;
(ert-deftest shu-test-shu-add-to-alist-1 ()
  (let ((alist)
        (item (cons "A" 0))
        (added-item))
    (shu-add-to-alist added-item item alist)
    (should added-item)
    (should (consp added-item))
    (should (stringp (car added-item)))
    (should (numberp (cdr added-item)))
    (should (string= (car item) (car added-item)))
    (should (= (cdr item) (cdr added-item)))
    (should (eq added-item item))
    ))



;;
;;  shu-test-shu-add-to-alist-2
;;
(ert-deftest shu-test-shu-add-to-alist-2 ()
  (let ((alist)
        (item (cons "A" 0))
        (item2 (cons "B" 0))
        (added-item))
    (shu-add-to-alist added-item item alist)
    (shu-add-to-alist added-item item2 alist)
    (should added-item)
    (should (consp added-item))
    (should (stringp (car added-item)))
    (should (numberp (cdr added-item)))
    (should (string= (car item2) (car added-item)))
    (should (= (cdr item2) (cdr added-item)))
    (should (eq added-item item2))
    ))



;;
;;  shu-test-shu-add-to-alist-3
;;
(ert-deftest shu-test-shu-add-to-alist-3 ()
  (let ((alist)
        (item (cons "A" 0))
        (item2 (cons "B" 0))
        (item3 (cons "B" 0))
        (added-item))
    (shu-add-to-alist added-item item alist)
    (shu-add-to-alist added-item item2 alist)
    (shu-add-to-alist added-item item2 alist)
    (should added-item)
    (should (consp added-item))
    (should (stringp (car added-item)))
    (should (numberp (cdr added-item)))
    (should (string= (car item3) (car added-item)))
    (should (= (cdr item3) (cdr added-item)))
    (should (not (eq added-item item3)))
    ))



;;
;;  shu-test-shu-add-to-alist-4
;;
(ert-deftest shu-test-shu-add-to-alist-4 ()
  (let ((alist)
        (items (list "A" "B" "C" "A" "B"))
        (item)
        (count)
        (expected
         (list
          (cons "A" 2)
          (cons "B" 2)
          (cons "C" 1)))
        (actual)
        (x)
        (added-item))
    (while items
      (setq x (car items))
      (setq item (cons x 0))
      (shu-add-to-alist added-item item alist)
      (setq count (cdr added-item))
      (setq count (1+ count))
      (setcdr added-item count)
      (setq items (cdr items)))
    (should alist)
    (should (listp alist))
    (should (= 3 (length alist)))
    (setq actual (sort alist
                       (lambda(obj1 obj2)
                         (string< (car obj1) (car obj2)))))
    (should (equal expected actual))
    ))




;;
;;  shu-test-shu-invert-list-list-1
;;
(ert-deftest shu-test-shu-invert-list-list-1 ()
  (let ((classes
         (list
          (cons "A" (list "X" "Y" "Z"))
          (cons "B" (list "Q" "X" "Z"))
          (cons "C" (list "P" "X"))))
        (x)
        (actual)
        (expected
         (list
          (cons "P" (list "C"))
          (cons "Q" (list "B"))
          (cons "X" (list "A" "B" "C"))
          (cons "Y" (list "A"))
          (cons "Z" (list "A" "B")))))
    (setq x (shu-invert-list-list classes 'string<))
    (setq actual (sort x (lambda(lhs rhs) (string< (car lhs) (car rhs)))))
    (should (equal expected actual))
    ))




;;
;;  shu-test-shu-invert-list-list-2
;;
(ert-deftest shu-test-shu-invert-list-list-2 ()
  (let ((classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "world" (list "Hello" "Goodbye" "string" "set" "map"))))
        (x)
        (actual)
        (expected
         (list
          (cons "Goodbye" (list "world"))
          (cons "Hello"   (list "world"))
          (cons "map"     (list "world" "std"))
          (cons "set"     (list "world" "std"))
          (cons "string"  (list "world" "std"))
          (cons "vector"  (list "std")))))
    (setq x (shu-invert-list-list classes))
    (setq actual (sort x (lambda(lhs rhs) (string< (car lhs) (car rhs)))))
    (should (equal expected actual))
    ))



;;
;;  shu-test-shu-invert-list-list-3
;;
(ert-deftest shu-test-shu-invert-list-list-3 ()
  (let ((classes
         (list
          (cons "A" (list "X"))))
        (x)
        (actual)
        (expected
         (list
          (cons "X" (list "A")))))
    (setq x (shu-invert-list-list classes 'string<))
    (setq actual (sort x (lambda(lhs rhs) (string< (car lhs) (car rhs)))))
    (should (equal expected actual))
    ))


;;; shu-base.t.el ends here
