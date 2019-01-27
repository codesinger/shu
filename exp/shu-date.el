;;; shu-date.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-jdate
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

;;
;; A miscellaneous collection of useful functions for dealing with dates.
;;
;; A shu date is the corresponding julian day number as defined below.
;;
;; A shu time is a cons cell in which the car is seconds since midnight
;; and the cdr is microseconds.
;;
;; A shu datetime is a cons cell in which the car is the date and the
;; cdr is the time.


;;; Code:

(provide 'shu-date)

;;
;;
;;
;;
;;      Date:
;;
;;      is a single cons cell.  The sentinel is used for validation.
;;      The serial day is as defined by shu-jday / shu-jdate below.
;;
;;
;;       ----------------------------
;;       |             |            |
;;       |   Sentinel  | Serial Day |
;;       |             |            |
;;       ----------------------------
;;
;;
;;
;;
;;
;;      Time:
;;
;;      is a single cons cell.  The sentinel is used for validation.
;;      The Time Cons is a cons cell as defined below:
;;
;;
;;       ----------------------------
;;       |             |            |
;;       |   Sentinel  | Time Cons  |
;;       |             |            |
;;       ----------------------------
;;
;;
;;          Time Cons:
;;
;;          is a single cons cell.
;;          Seconds is seconds since midnight.
;;          Microsecs is the microseconds part of the time
;;
;;
;;           ----------------------------
;;           |             |            |
;;           |   Seconds   | Microsecs  |
;;           |             |            |
;;           ----------------------------
;;
;;
;;
;;
;;
;;
;;
;;      Datetime:
;;
;;      is a single cons cell.  The sentinel is used for validation.
;;      The DT Cons is a cons cell as defined below:
;;
;;
;;       ----------------------------
;;       |             |            |
;;       |   Sentinel  |   DT Cons  |
;;       |             |            |
;;       ----------------------------
;;
;;
;;          DT Cons:
;;
;;          is a single cons cell.
;;          Date is a date as defined above
;;          Time is a time as defined above
;;
;;
;;           ----------------------------
;;           |             |            |
;;           |    Date     |    Time    |
;;           |             |            |
;;           ----------------------------
;;

(defconst shu-date-date-sentinel 184556
  "The sentinel that is put into a date for validation.")


(defconst shu-date-time-sentinel 186554
  "The sentinel that is put into a time for validation.")


;;
;;  shu-date-make-date
;;
(defmacro shu-date-make-date (date-cons serial-day)
  "DATE-CONS is the resulting date cons cell.  SERIAL-DAY is the serial day
as defined by shu-jday / shu-jdate."
  `(setq ,date-cons (cons shu-date-date-sentinel ,serial-day))
  )


;;
;;  shu-date-extract-date
;;
(defmacro shu-date-extract-date (serial-day date-cons)
  "DATE-CONS is cons cell that holds the date.  SERIAL-DAY is the serial day
as defined by shu-jday / shu-jdate."
  `(if (not (consp ,date-cons))
       (error "Date is not a cons cell")
     (if (not (numberp (car ,date-cons)))
         (error "Date sentinel is not a number")
       (if (/= shu-date-date-sentinel (car ,date-cons))
           (error "Incorrect date sentinel")
         (if (not (numberp (cdr ,date-cons)))
             (error "Serial day of date is not a number")
           (setq ,serial-day (car ,date-cons))
           )
         )
       )
     )
  )


;;
;;  shu-date-make-time
;;
(defmacro shu-date-make-time (time-cons seconds microseconds)
  "DATE-CONS is the resulting date cons cell.  SERIAL-DAY is the serial day
as defined by shu-jday / shu-jdate."
  (let (
        (ttime-cons (make-symbol "ttime-cons"))
        )
    `(let (
           (,ttime-cons)
           )
       (setq ,ttime-cons (cons ,seconds ,microseconds))
       (setq ,time-cons (cons shu-date-time-sentinel ,ttime-cons)))
    ))


;;
;;  shu-date-extract-time
;;
(defmacro shu-date-extract-time (seconds microseconds time-cons)
  "DATE-CONS is the resulting date cons cell.  SERIAL-DAY is the serial day
as defined by shu-jday / shu-jdate."
  (let (
        (ttime-cons (make-symbol "ttime-cons"))
        )
    `(let (
           (,ttime-cons)
           )
       (if (not (consp ,time-cons))
           (error "Time is not a cons cell")
         (if (not (numberp (car ,time-cons)))
             (error "Time sentinel is not a number")
           (if (/= shu-date-time-sentinel (car ,time-cons))
               (error "Incorrect time sentinel")
             (setq ,ttime-cons (cdr ,time-cons))
             (if (not (consp ,ttime-cons))
                 (error "cdr of time-cons is not a cons cell")
               (if (not (numberp (car ,ttime-cons)))
                   (error "seconds value of time is not a number")
                 (if (not (numberp (cdr ,ttime-cons)))
                     (error "microseconds value is not a number")
                   (setq ,seconds (car ,ttime-cons))
                   (setq ,microseconds (cdr ,ttime-cons))
                   )
                 )
               )
             )
           )
         )
       )
    ))


;;
;;  shu-date-make-datetime
;;
(defmacro shu-date-make-datetime (datetime-cons serial-day seconds microseconds)
  "DATE-CONS is the resulting date cons cell.  SERIAL-DAY is the serial day
as defined by shu-jday / shu-jdate."
  (let (
        (tdate-cons (make-symbol "tdate-cons"))
        (ttime-cons (make-symbol "ttime-cons"))
        )
    `(let (
           (,tdate-cons)
           (,ttime-cons)
           )
       (shu-date-make-date ,tdate-cons ,serial-day)
       (shu-date-make-time ,ttime-cons ,seconds ,microseconds)
       (setq ,datetime-cons (cons ,tdate-cons ,ttime-cons)))
    ))


;;
;;  shu-date-extract-datetime
;;
(defmacro shu-date-extract-datetime (serial-day seconds microseconds datetime-cons)
  "DATE-CONS is the resulting date cons cell.  SERIAL-DAY is the serial day
as defined by shu-jday / shu-jdate."
  `(if (not (consp ,datetime-cons))
       (error "datetime is not a cons cell")
     (shu-date-extract-date ,serial-day (car ,datetime-cons))
     (shu-date-extract-time ,seconds ,microseconds (cdr ,datetime-cons))
     )
  )





;;
;;  shu-convert-to-datetime-1
;;
(defun shu-convert-to-datetime-1 (date-time-string)
  "Convert a string of the form yyyy-mm-ddThhmmss.mmm to a shu datetime."
  (let (
        (dtrx
         (concat
          "\\([0-9]\\{4\\}\\)"   ;; 1 - Year
          "-"
          "\\([0-9]\\{2\\}\\)"   ;; 2 - Month
          "-"
          "\\([0-9]\\{2\\}\\)"   ;; 3 - Day
          "T"
          "\\([0-9]\\{2\\}\\)"   ;; 4 - Hour
          "\\([0-9]\\{2\\}\\)"   ;; 5 - Minute
          "\\([0-9]\\{2\\}\\)"   ;; 6 - Second
          "."
          "\\([0-9]\\{3\\}\\)")) ;; 7 - Milliseconds
        )
    (shu-internal-convert-to-datetime-1 date-time-string dtrx)
    ))



;;
;;  NB: NEITHER FUNCTION BELOW USE THE DATE AND TIME FORMATS
;;      OUTLINED ABOVE.  STILL TO BE DONE TO MAKE THESE USE
;;      THE ABOVE FORMATS
;;

;;
;;  shu-inrternal-convert-to-datetime-1
;;
(defun shu-internal-convert-to-datetime-1 (date-time-string rx-parse)
  "Convert a string to a shu datetime.  DATE-TIME-STRING is the string to be
converted.  RX-PARSE is a regular expression used to parse the string.  The
string may be in any format that can be parsed by RX-PARSE.  The strings
produced by the string-match ust be as follows:

   1 - four digit year
   2 - month
   3 - day
   4 - hour
   5 - minute
   6 - second
   7 - milliseconds"
  (let (
        (dts date-time-string)
        (yy)
        (mm)
        (dd)
        (hh)
        (min)
        (ss)
        (mil)
        (jd)
        (st)
        (serial-time)
        (date-time)
        (case-fold-search nil)
        )
    (setq debug-on-error t)
    (when (string-match rx-parse dts)
      (setq yy (string-to-number (match-string 1 dts)))
      (setq mm (string-to-number (match-string 2 dts)))
      (setq dd (string-to-number (match-string 3 dts)))
      (setq hh (string-to-number (match-string 4 dts)))
      (setq min (string-to-number (match-string 5 dts)))
      (setq ss (string-to-number (match-string 6 dts)))
      (setq mil (string-to-number (match-string 7 dts)))
      (setq jd (shu-jday (list yy mm dd)))
      (setq st (+ (* hh 60 60) (* min 60) ss))
      (setq serial-time (cons st (* 1000 mil)))
      (setq date-time (cons jd serial-time))
      )
    date-time
    ))



;;
;;  shu-format-datetime-1
;;
(defun shu-format-datetime-1 (date-time)
  "Convert a shu datetime to a string of the form yyyy-mm-ddThhmmss.mmm."
  (let* (
         (serial-day (car date-time))
         (serial-time (cdr date-time))
         (seconds (car serial-time))
         (mills (shu-micros-to-millis (cdr serial-time)))
         (date-list (shu-jdate serial-day))
         (year (car date-list))
         (month (cadr date-list))
         (day (caddr date-list))
         (zs (shu-seconds-to-hhmmss (car serial-time)))
         (hour (car zs))
         (minute (cadr zs))
         (sec (caddr zs))
         (year-string (shu-format-num year 4 ?0))
         (month-string (shu-format-num month 2 ?0))
         (day-string (shu-format-num day 2 ?0))
         (hour-string (shu-format-num hour 2 ?0))
         (minute-string (shu-format-num minute 2 ?0))
         (sec-string (shu-format-num sec 2 ?0))
         (mill-string (shu-format-num mills 3 ?0))
         (ftime
          (format "%s-%s-%sT%s%s%s.%s"
                  year-string month-string day-string
                  hour-string minute-string sec-string
                  mill-string))
         )
    ftime
    ))



;;
;;  shu-mon-to-number
;;
(defun shu-mon-to-number (mon)
  "Turn a three character month (\"jan\", \"feb\", \"mar\", etc.) into its
respective month number."
  (let (
        (mm (downcase mon))
        (num 0)
        (x)
        )
    (setq x (assoc mm shu-date-mon-num-tab))
    (when x
      (setq num (cdr x))
      )
    num
    ))

(defconst shu-date-mon-num-tab
  (list
   (cons "jan"  1) (cons "feb"  2) (cons "mar"  3) (cons "apr"  4)
   (cons "may"  5) (cons "jun"  6) (cons "jul"  7) (cons "aug"  8)
   (cons "sep"  9) (cons "oct" 10) (cons "nov" 11) (cons "dec" 12))
  "Alist used to convert 3 character month names to month numbers.")


(defconst shu-date-num-mon-tab
  ["jan" "feb" "mar" "apr"
   "may" "jun" "jul" "aug"
   "sep" "oct" "nov" "dec"]
  "Array used to convert month numbers to three character month names.")



;;
;;  shu-number-to-mon
;;
(defun shu-number-to-mon (num)
  "Turn a momth umber into its three character abbreviation."
  (interactive)
  (let ((x)
        (mon "???"))
    (when (and (> num 0) (< num 13))
      (setq x (1- num))
      (setq mon (aref shu-date-num-mon-tab x)))
    mon
    ))



;;
;;  shu-seconds-to-hhmmss
;;
(defsubst shu-seconds-to-hhmmss (serial-seconds)
  "Convert seconds since midnight into a list of hours, minutes, seconds."
  (let* ((hh (/ serial-seconds (* 60 60)))
         (rs (- serial-seconds (* hh 60 60)))
         (mm (/ rs 60))
         (ss (- rs (* mm 60))))
    (list hh mm ss)
    ))



;;
;;  shu-micros-to-millis
;;
(defsubst shu-micros-to-millis (microseconds)
  "Convert microseconds to milliseconds with rounding"
  (let* ((mills (/ microseconds 1000))
         (xmills (if (> (- microseconds (* mills 1000)) 499)
                     (1+ mills)
                   mills)))
    xmills
    ))



(defconst shu-jdate-const1 1721119
  "A constant used in jdate calculations")

(defconst shu-jdate-const2 146097
  "A constant used in jdate calculations")

(defconst shu-jdate-const3 1461
  "A constant used in jdate calculations")

(defconst shu-jdate-const4 153
  "A constant used in jdate calculations")


;;
;;  shu-jdate
;;
(defun shu-jdate (serial-day)
  "Convert a serial julian day number to year, month, day.  Return year,
month, day in a list.
See Collected Algorithms of the ACM, Algorithm 199, by Robert G. Tantzen
Commun. ACM 6(8): 444 (1963)"
  (let ((j (- serial-day shu-jdate-const1))
        (y)
        (m)
        (d))
    (setq y (/ (1- (* j 4)) shu-jdate-const2))
    (setq j (- (1- (* j 4)) (* y shu-jdate-const2)))
    (setq d (/ j 4))
    (setq j (/ (+ (* d 4) 3) shu-jdate-const3))
    (setq d (- (+ (* d 4) 3) (* j shu-jdate-const3)))
    (setq d (/ (+ d 4) 4))
    (setq m (/ (- (* 5 d) 3) shu-jdate-const4))
    (setq d (- (- (* d 5) 3) (* m shu-jdate-const4)))
    (setq d (/ (+ d 5) 5))
    (setq y (+ (* 100 y) j))
    (if (< m 10)
        (setq m (- m 3))
      (setq m (- m 9))
      (setq y (1+ y)))
    (list y m d)
    ))




;;
;;  shu-jday
;;
(defun shu-jday (date)
  "Convert a list containing year, month, day into a serial jiulian day number.
See Collected Algorithms of the ACM, Algorithm 199, by Robert G. Tantzen
Commun. ACM 6(8): 444 (1963)"
  (interactive)
  (let ((y (car date))
        (m (cadr date))
        (d (caddr date))
        (c)
        (j)
        (ya))
    (if (> m 2)
        (setq m (- m 3))
      (setq m (+ m 9))
      (setq y (1- y)))
    (setq c (/ y 100))
    (setq ya (- y (* 100 c)))
    (setq j (+
             (/ (* c shu-jdate-const2) 4)
             (/ (* ya shu-jdate-const3) 4)
             (/ (+ (* m shu-jdate-const4) 2) 5)
             d
             shu-jdate-const1))
    ))


;;; shu-date.el ends here
