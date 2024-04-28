;;; shu-batch-compile-directive.el --- Shu project batch compiler directive
;;
;; Copyright (C) 2024 Stewart L. Palmer
;;
;; Package: shu-base
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;; Homepage: https://github.com/codesinger/shu.git
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

;;; Commentary:

;; This file tells the compiler to add the current directory to the load path
;; at compile time.  If all of the lisp code in the shu project is being
;; compiled in batch mode with batch-byte-compile, the files must be compiled
;; in order of dependencies and the compiler must understand the load path at
;; compile time.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path default-directory))

;;; shu-batch-compile-directive.el ends here
