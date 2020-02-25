;;; shu-new3.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2020 Stewart L. Palmer
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

;; A collection of experimental functions for dealing with C++ code.
;;
;;

;;; Code:






;;
;;  shu-tighten-lisp-old
;;
(defun shu-tighten-lisp-old ()
  "Within the bounds of a lisp function or macro, \"tighten\" some lisp code.
Look for any single right parenthesis that is on its own line and move it up to
the end of the previous line.  This function is the opposite of SHU-LOOSEN-LISP"
  (interactive)
  (let ((ssfun
         (concat
          "("
          "\\s-*"
          "\\(defun\\|defsubst\\|defmacro\\|ert-deftest\\|defvar\\|defconst\\)"))
        (bof)
        (eof)
        (ss "\\s-+)$")
        (ss2 "\\s-+($")
        (bob)
        (eob)
        (del-count))
    (save-excursion
      (if (not (re-search-backward ssfun nil t))
          (progn
            (ding)
            (message "%s" "Not inside a macro or function"))
        (setq bof (match-beginning 0))
        (setq eof (shu-point-at-sexp bof))
        (goto-char bof)
        (while (re-search-forward ss eof t)
          (setq eob (1- (point)))
          (forward-line -1)
          (end-of-line)
          (setq bob (point))
          (delete-region bob eob)
          (setq eof (shu-point-at-sexp bof)))
        (goto-char bof)
        (while (re-search-forward ss2 eof t)
          (setq bob (point))
          (forward-line 1)
          (when (re-search-forward "^\\s-+(")
            (setq eob (1- (point)))
            (delete-region bob eob)
            (setq del-count (1+ (- eob bob)))
            (setq eof (- eof del-count))))))
    ))



;;
;;  shu-misc-rx-lets
;;
(defconst shu-misc-rx-lets
  (concat
          "("
          "\\s-*"
          "\\(let"
          "\\|let\\*"
          "\\)"
          shu-all-whitespace-regexp "*"
          "\\((\\)"
          )
  "Rregular expression to find the beginning of a let soecial form.")



;;
;;  shu-tighten-lisp
;;
(defun shu-tighten-lisp ()
  "Within the bounds of a lisp function or macro, \"tighten\" some lisp code.
Look for any single right parenthesis that is on its own line and move it up to
the end of the previous line.  This function is the opposite of SHU-LOOSEN-LISP"
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (ret-val)
        (bof)
        (eof)
        (doing t)
        (p)
        (start-pos)
        (end-pos)
        (length)
        (pad)
        (pad-length)
        (start-col)
        (let-begin)
        (sp)
        (xx (concat shu-all-whitespace-regexp "*" ")"))
        (zz (concat shu-all-whitespace-regexp "*" "("))
        (debug-on-error t)
        )
    (save-excursion
      (setq ret-val (shu-get-containing-functino))
      (when ret-val
        (setq bof (car ret-val))
        (setq eof (cdr ret-val))
        (princ (format "bof: %d, eof: %d\n" bof eof) gb)
        ;; Handle all containing functions other than "let"
        (goto-char bof)
        (while doing
          (if (not (re-search-forward shu-misc-rx-conditionals eof t))
              (setq doing nil)
            (setq p (1- (point)))
            (princ (format "Found(x): %s at %d\n" (match-string 0) (point)) gb)
            (when (search-backward "(" nil t)
              (setq eof (fix1 eof))
              (goto-char p)
              )
            )
          )
        ;; Handle "let" and "let*"
        (goto-char bof)
        (while (re-search-forward shu-misc-rx-lets eof t)
          (princ (format "Found(1): %s at %d and %s at %d\n" (match-string 1) (match-beginning 1) (match-string 2) (match-beginning 2)) gb)
          (setq start-pos (point))
          (backward-char 1)
          (setq eof (fix1 eof))
          (goto-char start-pos)
          (when (re-search-forward zz eof t)
            (setq end-pos (1- (point)))
            (setq length (- end-pos start-pos))
            (when (> length 0)
              (delete-region start-pos end-pos)
              (setq eof (- eof length))

              )
            )
          )


        )
      )
    ))



;;
;;  zz
;;
(defun zz ()
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (data
         (concat
          "(defun zz ()\n"
          "  (interactive)\n"
          "  (let (\n"
          "        )\n"
          "    (when x\n"
          "      (setq x (1- x))\n"
          "      )\n"
          "    ))\n"
          ))
        (actual)
        )
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (search-forward "let" nil t)
      (princ (format "point-max1: %d\n" (point-max)) gb)
      (shu-tighten-lisp)
      (princ (format "point-max2: %d\n" (point-max)) gb)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (princ (concat "\n" actual "\n" )gb)

      )
    ))



;;
;;  qq
;;
(defun qq ()
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (data
         (concat
          "(defun zz ()\n"
          "  (interactive)\n"
          "  (let (\n"
          "        )\n"
          "    (when x\n"
          "      (setq x (1- x))\n"
          "      \n"
          "      \n"
          "      )\n"
          "    ))\n"
          ))
        (eof)
        (xx (concat shu-all-whitespace-regexp "*" ")"))
        (actual)
        (p)
        )
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq eof (point-max))
      (search-forward "let" nil t)
      (when (re-search-forward shu-misc-rx-conditionals eof t)
        (princ (format "Found(1) %s at %d, point: %d\n" (match-string 1) (match-beginning 1)(point)) gb)
        (search-backward "(" nil t)
        (setq p (point))
        (forward-sexp)
        (backward-char 1)
        (when (re-search-backward xx nil t)
          (princ (format "Found(2) %s at %d, point: %d\n" (match-string 0) (match-beginning 0)(point)) gb)

          )

          )
      )
    ))


;;
;;  rr
;;
(defun rr ()
  "Doc string."
  (interactive)
  (let (
        (xx (concat shu-all-whitespace-regexp "*" ")"))
        (eof (point-max))
        )
    (forward-sexp)
    (backward-char 1)
    (re-search-backward xx nil t)
    ))


;;
;;  ss
;;
(defun ss ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create  "**boo**"))
        (xx (concat shu-all-whitespace-regexp "*" ")"))
        (eof (point-max))
        )
    (when (re-search-forward shu-misc-rx-conditionals eof t)
      (when (search-backward "(" nil t)
        (forward-sexp)
        (backward-char 1)

          (princ (format "sexp at %d\n" (point)) gb)
          (setq sp (shu-starts-with ")"))
          (when sp
            (princ "Starts with )\n" gb)
            (setq end-pos sp)
            (princ (format "end-pos: %d\n" end-pos) gb)
            (when (re-search-backward xx nil t)
                  (setq start-pos (1+ (point)))
                  (setq length (- end-pos start-pos))
                  (princ (format "start-pos: %d, end-pos: %d, length: %d\n" start-pos end-pos length) gb)
               )
            )

        )


      )
    ))





;;
;;  shu-loosen-lisp
;;
(defun shu-loosen-lisp ()
  "Within the bounds of a lisp function, unwind the parentheses that terminate
conditional and containing functions such that it is convenient to insert code
inside of them without having to worry about which line contains the closing
parenthesis.  All closing parentheses are now on separate lines.  Once the
changes to the function are complete, you can run SHU-TIGHTEN-LISP to put the
parentheses back where they belong."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (ret-val)
        (bof)
        (eof)
        (doing t)
        (p)
        (pad)
        (pad-length)
        (start-col)
        (let-begin)
          )
    (save-excursion
      (setq ret-val (shu-get-containing-functino))
      (when ret-val
        (setq bof (car ret-val))
        (setq eof (cdr ret-val))
        (princ (format "bof: %d, eof: %d\n" bof eof) gb)
        ;; Handle all containing functions other than "let"
        (goto-char bof)
        (while doing
          (if (not (re-search-forward shu-misc-rx-conditionals eof t))
              (setq doing nil)
            (princ (format "FounE_d: %s at %d\n" (match-string 0) (point)) gb)
            (setq p (1- (point)))
            (goto-char (match-beginning 0))
            (setq start-col (current-column))
            (beginning-of-line)
            (forward-sexp)
            (backward-char 1)
            (setq pad-length (+ start-col 2))
            (setq pad (concat "\n" (make-string pad-length ? )))
            (insert pad)
            (setq eof (+ eof (length pad)))
            (goto-char p)
            )
          )
        ;; Handle "let" and "let*"
        (goto-char bof)
        (while (re-search-forward shu-misc-rx-lets eof t)
            (setq let-begin (match-beginning 0))
            (setq p (point))
            (princ (format "p: %d\n" p ) gb)
            (setq start-col (current-column))
            (setq pad-length start-col)
            (setq pad (concat "\n" (make-string pad-length ? )))
            (insert pad)
            (setq eof (+ eof (length pad)))
            (goto-char p)
            (when (re-search-backward "(\\s-*" let-begin t)
              (forward-sexp)
              (backward-char 1)
              (setq pad-length (+ start-col 2))
              (setq pad (concat "\n" (make-string pad-length ? )))
              (insert pad)
              (setq eof (+ eof (length pad)))
              )
          )
        )
      )
         ))



;; TODO pass in the value of eof as an input parameter and have this
;;      function return the updatedd value of eof.
;;
;;  fix1
;;
(defun fix1 (eof)
  "Call this function while point is on a left parenthesis.  This function will
find the matching right parenthesis.  If the matching right parenthesis is on a line
by itself and a previous line ends in another right parenthesis, the line and
dangling right parenthesis will be moved up to the end of the line that also ends
in a right parenthesis.  This is an internal part of the function SHU-TIGHTEN-LISP.
EOF is the point at which the current function on which we are operating ends.
This function removes some text from the current function.  It adjusts EOF appropriately
and reeturns thee new value to the caller.."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (sp)
        (end-pos)
        (start-pos)
        (xx (concat shu-all-whitespace-regexp "*" ")"))
        (p)
        (length)
        )
    (forward-sexp)
    (backward-char 1)
    (princ (format "sexp at %d\n" (point)) gb)
    (setq sp (shu-starts-with ")"))
    (when sp
      (princ "Starts with )\n" gb)
      (setq end-pos sp)
      (princ (format "end-pos: %d\n" end-pos) gb)
      (when (re-search-backward xx p t)
        (setq start-pos (1+ (point)))
        (setq length (- end-pos start-pos))
        (princ (format "start-pos: %d, end-pos: %d, length: %d\n" start-pos end-pos length) gb)
        (delete-region start-pos end-pos)
        (setq eof (- eof length))
        )
      )
    eof
    ))




;;
;;  lt
;;
(defun lt ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (eof (point-max))
        (let-begin)
        (start-pos)
        (end-pos)
        (zz (concat shu-all-whitespace-regexp "*" "("))
        )
        (when (re-search-forward shu-misc-rx-lets eof t)
          (princ (format "Found(1): %s at %d and %s at %d\n" (match-string 1) (match-beginning 1) (match-string 2) (match-beginning 2)) gb)
          (setq start-pos (point))
          (backward-char 1)
          (setq eof (fix1 eof))
          (goto-char start-pos)
          (when (re-search-forward zz eof t)
            (setq end-pos (1- (point)))
            (setq length (- end-pos start-pos))
            (when (> length 0)
              (delete-region start-pos end-pos)
              (setq eof (- eof length))

              )
            )
          )

    ))


;;; shu-new3.el ends here
