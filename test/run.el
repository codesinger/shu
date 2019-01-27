(defun jj ()
  (interactive)
  (shu-run-all-unit-tests)
  )

;;
;;  shu-run-all-unit-tests
;;
(defun shu-run-all-unit-tests ()
  "Compile and run all of the unit tests.  This must be run from the test directory."
  (interactive)
  (let
      ((test-files
        (list
;;;         "find.t.elc"
;;;         "macros.t.elc"
;;;         "s-mode.t.elc"
         "shu-base.t.elc"
         "shu-capture-doc.t.elc"
         "shu-cpp-general.t.elc"
         "shu-cpp-project.t.elc"
         "shu-cpp-token.t.elc"
         "shu-misc.t.elc"
         "shu-nvplist.t.elc"
         "shu-xref.t.elc"))
       (file-name)
       (tlist))
    (byte-force-recompile ".")
    (setq tlist test-files)
    (while tlist
      (setq file-name (car tlist))
      (load-file file-name)
      (setq tlist (cdr tlist)))
    (ert t)
    ))
