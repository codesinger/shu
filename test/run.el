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
;;;         "find.t.el"
         "macros.t.el"
         "s-mode.t.el"
         "shu-base.t.el"
         "shu-capture-doc.t.el"
         "shu-cpp-general.t.el"
         "shu-cpp-project.t.el"
         "shu-cpp-token.t.el"
         "shu-nvplist.t.el"
         "shu-xref.t.el"))
       (file-name)
       (tlist))
    (setq tlist test-files)
    (while tlist
      (setq file-name (car tlist))
      (byte-recompile-file file-name 0 0 1)
      (setq tlist (cdr tlist)))
    (ert t)
    ))
