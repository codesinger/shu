
(require 'ert)





;;
;;  shu-cpp-find-using
;;
(defun shu-cpp-find-using (&optional top-name)
  "Return the name of the class found on the next \"using namespace\" directive
or nil of no such directive found.

TOP-NAME, if present is a higher level namespace.  Given a top level namespace
of \"WhammoCorp\", then the following line:

     using namespace WhammoCorp::world;

would be interpreted as though it had been written:

     using namespace world;"
  (interactive)
  (let ((using "using\\s-+namespace\\s-+\\([a-zA-Z0-9:_$]+\\)\\s-*;")
        (looking t)
        (top-qual (when top-name (concat top-name "::\\([a-zA-Z0-9_$]+\\)")))
        (name)
        (using-name)
        (mbeg)
        (bol)
        (not-comment))
    (while looking
      (setq using-name nil)
      (setq not-comment nil)
      (if (not (re-search-forward using nil t))
          (setq looking nil)
        (setq name (match-string 1))
        (setq mbeg (match-beginning 0))
        (setq bol (line-beginning-position))
        (save-match-data
          (save-excursion
            (when (not (shu-point-in-string (1- (point))))
              (setq not-comment t)
              (goto-char bol)
              (when (search-forward "//" mbeg t)
                (setq not-comment nil)))
            (when not-comment
              (when top-qual
                (when (string-match top-qual name)
                  (setq name (match-string 1 name)))))
            (when not-comment
              (setq using-name name)
              (setq looking nil))))))
    using-name
    ))



;;
;;  shu-test-shu-cpp-find-using-1
;;
(ert-deftest shu-test-shu-cpp-find-using-1 ()
  (let ((data
         (concat
          "\ninclude <something.h>\n"
          "using namespace glory;\n"))
        (expected "glory")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-find-using-2
;;
(ert-deftest shu-test-shu-cpp-find-using-2 ()
  (let ((data
         (concat
          "\ninclude <something.h>\n"
          " \"using namespace glory;\"\n"))
        (expected "glory")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq actual (shu-cpp-find-using))
      (should (not actual)))
    ))



;;
;;  shu-test-shu-cpp-find-using-3
;;
(ert-deftest shu-test-shu-cpp-find-using-3 ()
  (let ((data
         (concat
          "\ninclude <something.h>\n"
          "using namespace glory;\n"
          "using namespace bob;\n"
          "using namespace fred;\n"))
        (expected1 "glory")
        (expected2 "bob")
        (expected3 "fred")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected1 actual))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected2 actual))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected3 actual)))
    ))



;;
;;  shu-test-shu-cpp-find-using-4
;;
(ert-deftest shu-test-shu-cpp-find-using-4 ()
  (let ((data
         (concat
          "\ninclude <something.h>\n"
          "using namespace glory;\n"
          "// using namespace bob;\n"
          "using namespace fred;\n"))
        (expected1 "glory")
        (expected2 "fred")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected1 actual))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected2 actual)))
    ))



;;
;;  shu-test-shu-cpp-find-using-5
;;
(ert-deftest shu-test-shu-cpp-find-using-5 ()
  (let ((data
         (concat
          "\ninclude <something.h>\n"
          "using namespace glory;\n"
          "\"using namespace bob;\"\n"
          "using namespace fred;\n"))
        (expected1 "glory")
        (expected2 "fred")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected1 actual))
      (setq actual (shu-cpp-find-using))
      (should actual)
      (should (stringp actual))
      (should (string= expected2 actual)))
    ))



;;
;;  shu-test-shu-cpp-find-using-6
;;
(ert-deftest shu-test-shu-cpp-find-using-6 ()
  (let ((data
         (concat
          "\ninclude <something.h>\n"
          "using namespace glory;\n"
          "\"using namespace bob;\"\n"
          "using namespace WhammoCorp::fred;\n" ))
        (top-name "WhammoCorp")
        (expected1 "glory")
        (expected2 "fred")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq actual (shu-cpp-find-using top-name))
      (should actual)
      (should (stringp actual))
      (should (string= expected1 actual))
      (setq actual (shu-cpp-find-using top-name))
      (should actual)
      (should (stringp actual))
      (should (string= expected2 actual)))
    ))




;;
;;  shu-cpp-rmv-using
;;
(defun shu-cpp-rmv-using (class-list &optional top-name)
  "Remove \"using namespace\" directives from a C++ file, adding the appropriate
namespace qualifier to all of the unqualified class names.  CLASS-LIST is an
a-list in which the car of each entry is a namespace and the cdr of each entry
is a class name.  Here is an example of such an a-list:

     (list
      (cons \"std\"    (list \"set\" \"string\" \"vector\"))
      (cons \"world\"  (list \"Hello\" \"Goodbye\")))

TOP-NAME, if present is a higher level namespace.  Given a top level namespace
of \"WhammoCorp\", then the following line:

     using namespace WhammoCorp::world;

would be interpreted as though it had been written:

     using namespace world;"
  (let* ((gb-name "**shu-chgs**")
         (gb (get-buffer-create gb-name))
         (using "using\\s-+namespace\\s-+\\([a-zA-Z0-9:_$]+\\)\\s-*;")
         (top-qual (when top-name (concat top-name "::\\([a-zA-Z0-9_$]+\\)")))
         (bol)
         (ct 0)
         (count 0)
         (uc 0)
         (unk "")
         (name)
         (mbeg)
         (not-comment)
         (x)
         (classes)
         (namespace)
         (debug-on-error t)
         (case-fold-search nil))
    (if (shu-cpp-rmv-blocked class-list using top-qual gb)
        (progn
          (ding)
          (message "Class ambiguity prevents change.  See buffer %s" gb-name))
      (goto-char (point-min))
      (while (re-search-forward using nil t)
        (setq name (match-string 1))
        (setq mbeg (match-beginning 0))
        (setq bol (line-beginning-position))
        (save-match-data
          (save-excursion
            (setq not-comment t)
            (goto-char bol)
            (when (search-forward "//" mbeg t)
              (setq not-comment nil)
              )
            )
          )
          (setq x (assoc name class-list))
          (if (not x)
              (progn
                (princ (format "Unknown namespace: \"%s\"\n" name) gb)
                (setq uc (1+ uc)))
            (delete-region (line-beginning-position) (line-end-position))
            (setq namespace (car x))
            (setq classes (cdr x))
            (save-excursion
              (setq ct (shu-cpp-qualify-classes classes namespace gb)))
            (setq count (+ count ct))
            )

        )
      (goto-char (point-min))
      (when (not (= 0 uc))
        (setq unk (format " %d unknown namespaces. " uc)))
      (message "Replaced %d occurrences.%s  See buffer **chgs**" count unk)
      )
    count
    ))



;;
;;  shu-cpp-rmv-blocked
;;
(defun shu-cpp-rmv-blocked (class-list using top-name gb)
  "Do a pre-check on a file to see if we will be able to remove its \"using
namespace\" directives.  CLASS-LIST is the a-list passed to SHU-CPP-RMV-USING.
USING is the regular expression used to search for \"using namespace\"
directives.  TOP-QUAL is the regular expression used to strip out a higher level
qualifier from the class name in a \"using namespace\" directive, if any.  GB is
the buffer into which diagnostic messages are written.

This function finds all of the \"using namespace\" directives in the file and
checks to see if there is any ambiguity in the resulting class list.  For
example, if namespace \"mumble\" contains class \"Bumble\" and namespace
\"stubble\" also contains class \"Bumble\", we will not know which namespace to
apply to instances of class \"Bumble\".  But this is not an ambiguity if there
is a \"using namespace\" directive for only one of those classes.  That is why
we do the ambiguity check only for namespaces referenced by \"using namespace\"
directives.

This function returns true if such an ambiguity exists."
  (let ((name)
        (mbeg)
        (bol)
        (x)
        (z)
        (uc 0)
        (looking)
        (item)
        (added-item)
        (duplicates)
        (clist)
        (cl)
        (ns)
        (classes)
        (class)
        (listc)
        (blocked))
    (save-excursion
      (goto-char (point-min))
      (setq looking t)
      (while looking
        (setq name (shu-cpp-find-using top-name))
        (if (not name)
            (setq looking nil)
          (setq mbeg (match-beginning 0))
          (setq item (cons name (line-number-at-pos mbeg)))
          (shu-add-to-alist added-item item duplicates)
          (when (eq added-item item) ;; Name is not duplicate
            (setq x (assoc name class-list))
            (when x
              (setq clist (cons x clist))
              )
            )
          )
        )
      )
    (setq cl clist)
    (while cl
      (setq x (car cl))
      (setq ns (car x))
      (setq classes (cdr x))
      (while classes
        (setq class (car classes))
        (setq x (cons class ns))
        (if (not listc)
            (setq listc (cons x listc))
          (setq z (assoc class listc))
          (if (not z)
              (setq listc (cons x listc))
            (princ (format "class %s in namespace %s conflicts with class %s in namespace %s\n"
                           class ns (car z) (cdr z)) gb)
            (setq blocked t)
            )
          )
        (setq classes (cdr classes))
        )
      (setq cl (cdr cl))
      )
    blocked
    ))





;;
;;  aaa
;;
(defun aaa ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (x)
        (added-item)
        (new-item)
        (alist)
        )
    (setq x (macroexpand '(shu-add-to-alist added-item new-item alist)))
    (princ x gb)

    ))
