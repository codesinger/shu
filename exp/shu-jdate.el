;;; shu-cpp-misc.el --- Shu project code for dealing wth C++ in Emacs
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

;; The functions in this file convert to and from julian dates
;;
;; See Collected Algorithms of the ACM, Algorithm 199, by Robert G. Tantzen
;; Commun. ACM 6(8): 444 (1963)


;;
;; A miscellaneous collection of useful functions

;;; Code:

(provide 'shu-jdate)

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
  "Convert a serial julian day number to month, day, year.  Return month,
day, year in a list."
  (let (
        (j (- serial-day shu-jdate-const1))
        (y)
        (m)
        (d)
        )
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


;;; shu-jdate.el ends here
