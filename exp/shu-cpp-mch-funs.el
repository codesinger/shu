;;; shu-cpp-mch-funs.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-mch-funs
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

;; A collection of useful functions for dealing with C++ code.
;;
;;

;;; Code:


(provide 'shu-cpp-mch-funs)
(require 'shu-cpp-match)




(defconst shu-cpp-mch-colon-name
  (list
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             nil shu-cpp-token-type-op
                             "::")
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                             'shu-cpp-token-match-same-rx
                             t shu-cpp-token-type-uq
                             (concat shu-cpp-name "+"))
   )
  "A repeating side list to match zero or more intances of {:: <name>}")


;;
;;  shu-cpp-mch-namespace-forms
;;
;;
;;                                  using
;;                                    |
;;                                    V
;;                                namespace
;;                                    |
;;                                    |
;;          +-------------------------+-------------------------+
;;          |                         |                         |
;;          |                         |                         |
;;          V                         V                         V
;;        <name>                     ::                       <name>
;;          |                         |                         |
;;          |                         |                         |
;;          |                         V                         V
;;          |                       <name>           loop of :: followed by <name>
;;          |                         |                         |
;;          |                         |                         |
;;          V                         V                         V
;;          ;                         ;                         ;
;;
;;
;;
(defconst shu-cpp-mch-namespace-forms
  (list
   (list  ;; "<name>"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                               (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                               ";")
    )


   (list  ;; ":: <name>"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              nil shu-cpp-token-type-op
                               "::")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                               (concat shu-cpp-name "+"))
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                               ";")
    )


   (list  ;; "<name> {:: <name>};"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                              'shu-cpp-token-match-same-rx
                              t shu-cpp-token-type-uq
                              (concat shu-cpp-name "+"))

    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-loop
                                                  shu-cpp-mch-colon-name)
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-op
                               ";")
    )
   )
  )


;;
;;  shu-cpp-mch-namespace-list
;;
(defconst shu-cpp-mch-namespace-list
  (list
   (list  ;; "using namespace <name>;"
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "using")
    (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                              'shu-cpp-token-match-same
                              t shu-cpp-token-type-kw
                              "namespace")
    (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                  shu-cpp-mch-namespace-forms)
    )
   )
  )



;;
;;  shu-cpp-mch-namespace-list-single
;;
(defconst shu-cpp-mch-namespace-list-single
  (list  ;; "using namespace <name>;"
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             t shu-cpp-token-type-kw
                             "using")
   (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                             'shu-cpp-token-match-same
                             t shu-cpp-token-type-kw
                             "namespace")
   (shu-cpp-make-match-side-list shu-cpp-token-match-type-side-many
                                 shu-cpp-mch-namespace-forms)
   )
  )

;;
;;
;;
;;
;;
;;
;;                                                                          using
;;                                                                            |
;;                                                                            |
;;                                   +----------------------------------------+-----------------------------*
;;                                   |                                                                      |
;;                                   |                                                                      |
;;                                   V                                                                      |
;;                               namespace                                                                  |
;;                                   |                                                                      |
;;                                   |                                                                      |
;;         +-------------------------+-------------------------+                           +----------------+---------------+
;;         |                         |                         |                           |                               |
;;         |                         |                         |                           |                               |
;;         V                         V                         V                           V                               V
;;       <name>                     ::                       <name>                      <name>                          <name>
;;         |                         |                         |                           |                               |
;;         |                         |                         |                           |                               |
;;         |                         V                         V                           |                               V
;;         |                       <name>           loop of :: followed by <na me>         |                    loop of :: followed by <name>
;;         |                         |                         |                           |                               |
;;         |                         |                         |                           |                               |
;;         V                         V                         V                           V                               V
;;         ;                         ;                         ;                           ;                               ;
;;





;;; shu-cpp-mch-funs.el ends here
