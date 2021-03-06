;;; shu-cpp-general.t.el --- Shu project code unit tests
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
(require 'shu-cpp-general)

;;; Code



(defconst shu-test-cpp-general-base-string
  (concat
   "\"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ"
   "RSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghijklmnopqrstuvwxyz12345678"
   "90!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ\"")
  ;; 12345678901234567890123456789012345678901234567890123456789012345678901234567890
  ;;          1         2         3         4         5         6         7         8
  "A test string for unit tests.")

(defconst shu-test-cpp-general-expected-split1
  (concat
   "\"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJK\"\n"
   "\"LMNOPQRSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghijklmnopqrstuv\"\n"
   "\"wxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ\"")
  "A test string for unit tests.")

(defconst shu-test-cpp-general--dox-cbt1-input
  (concat
   "     \n"
   "    //! Hello there this morning, how are you?\n")
  "A test input string for unit test of dox-cbt.")

(defconst shu-test-cpp-general-expected-dox-cbt1
  (concat
   "     \n"
   "    /*!\n"
   "     * \\brief Hello there this morning, how are you?\n"
   "     */\n")
  "A test output string for unit test of dox-cbt.")




;;
;;  shu-test-shu-dox-cbt-1
;;
(ert-deftest shu-test-shu-dox-cbt-1 ()
  (let ((result))
          ;; Split of one long line starting in column 1
  (with-temp-buffer
      (insert shu-test-cpp-general--dox-cbt1-input)
      (goto-char (point-min))
      (shu-dox-cbt)
      (should (= 67 (point)))
      ;; Make sure the result is what we expect
      (setq result (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= shu-test-cpp-general-expected-dox-cbt1 result)))
))



;;
;;  shu-test-shu-csplit-1
;;
(ert-deftest shu-test-shu-csplit-1 ()
  (let (
        (actual-split)
        (actual-unsplit)
        )
          ;; Split of one long line starting in column 1
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move to char right after quote
      (shu-csplit)             ;; Now try to split
      (should (= 215 (point))) ;; Point should have moved
      ;; Make sure the result is what we expect
      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (goto-char 20)
      ;; This should restore it to its original state
      (shu-cunsplit)
      (setq actual-unsplit (buffer-substring-no-properties 1 (1+ (length shu-test-cpp-general-base-string))))
      (should (string= shu-test-cpp-general-base-string actual-unsplit)))
))


;;
;;  shu-test-shu-csplit-2
;;
(ert-deftest shu-test-shu-csplit-2 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (base3 "\"abcdefg\"")
        )
      ;; Split of very short line starting in column 1
  (with-temp-buffer
      (insert base3)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 2)         ;; Move 2 chars right of quote
      (shu-csplit)             ;; Try to split again
      (should (= 10 (point)))  ;; Point should have moved
      ;; But nothing should have been split
      (setq actual-split (buffer-substring-no-properties 1 10))
      (should (string= base3 actual-split))
      (goto-char 4)
      (shu-cunsplit)           ;; Unsplit should do nothing either
      (setq actual-unsplit (buffer-substring-no-properties 1 (1+ (length base3))))
      (should (string= base3 actual-unsplit)))
))



;;
;;  shu-test-shu-csplit-3
;;
(ert-deftest shu-test-shu-csplit-3 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (expected-split2
         (concat "      \"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDE\"\n"
                 "      \"FGHIJKLMNOPQRSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghij\"\n"
                 "      \"klmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNO\"\n"
                 "      \"PQ\""))
        )
      ;; Split of one long line with 6 blanks in front of
  (with-temp-buffer
      (insert (concat "      " shu-test-cpp-general-base-string))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-csplit)             ;; Now try to split
      (should (= 242 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 242))
      (should (string= expected-split2 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 7 (+ 7 (length shu-test-cpp-general-base-string))))
      (should (string= shu-test-cpp-general-base-string actual-unsplit)))
))



;;
;;  shu-test-shu-csplit-4
;;
(ert-deftest shu-test-shu-csplit-4 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (base2
         (concat
          "\"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\""))
        ;;       12345678901234567890123456789012345678901234567890123456789012345678901234567890
        ;;                1         2         3         4         5         6         7         8
        (expected-split3
         (concat "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
                 "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
                 "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
                 "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
                 "      \"\\t\\t\\t\\t\\t\\t\\t\\t\""))
        )

  ;; Split a long string full of tab characters
  ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
  (with-temp-buffer
      (insert (concat "      " base2))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-csplit)             ;; Now try to split
      (should (= 179 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 179))
      (should (string= expected-split3 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 7 (+ 7 (length base2))))
      (should (string= base2 actual-unsplit)))
))



;;
;;  shu-test-shu-csplit-5
;;
(ert-deftest shu-test-shu-csplit-5 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (base2
         (concat
          "\"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\""))
        (expected-split4
         (concat
          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\""))
       )
      ;; Split a long string full of tab characters shifted right one from
  ;  previous test.  Put the "\" and "t" on different boundaries
  ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
  (with-temp-buffer
    (insert (concat "       " base2))
    (goto-char (point-min))  ;; Sitting in front of string
    (shu-csplit)             ;; Try to split
    (should (= 1 (point)))   ;; Nothing should have happened
    (forward-char 10)        ;; Move inside the string
    (shu-csplit)             ;; Now try to split
    (should (= 182 (point))) ;; Point should be here
    (setq actual-split (buffer-substring-no-properties 1 182))
    (should (string= expected-split4 actual-split))
    (goto-char 30)           ;; Go inside of top string
    (shu-cunsplit)           ;; Unsplit should restore to the original form
    (setq actual-unsplit (buffer-substring-no-properties 8 (+ 8 (length base2))))
    (should (string= base2 actual-unsplit)))
  ))




;;
;;  shu-test-shu-csplit-6
;;
(ert-deftest shu-test-shu-csplit-6 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (fail-case-unexpected
         (concat
          "\n"
          "        const std::string       expected(\"Lorem ipsum dolor sit amet, conse\"\n"
          "                                         \"ctetur adipiscing elit. Duis frin\"\n"
          "                                         \"gilla nunc eget ante ornare bland\"\n"
          "                                         \"it. Suspendis\");\n"
          "\n"
          "        EXPECT_EQ(expected, actual);\n"
          "        std::string x(\"unexpected stuff\");\n"
          "\n"
          "\n"))
        (fail-case-unexpected-after-split
         (concat
          "\n"
          "        const std::string       expected(\"Lorem ipsum dolor sit amet, consectetur "
          "adipiscing elit. Duis fringilla nunc eget ante ornare blandit. Suspendis\");\n"
          "\n"
          "        EXPECT_EQ(expected, actual);\n"
          "        std::string x(\"unexpected stuff\");\n"
          "\n"
          "\n"))
        )
    (with-temp-buffer
      (insert fail-case-unexpected)
      (goto-char (point-min))
      (search-forward "Lorem" nil t)
      (shu-cunsplit)
      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= fail-case-unexpected-after-split actual-split))
      )
    ))



;;
;;  shu-test-shu-cunsplit-1
;;
(ert-deftest shu-test-shu-cunsplit-1 ()
  "Doc string."
  (let ((data "\"A\" \"B\"")
        (expected "\"AB\"")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cunsplit-2
;;
(ert-deftest shu-test-shu-cunsplit-2 ()
  "Doc string."
  (let ((data "\"A\" \"B\"")
        (expected "\"AB\"")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cunsplit-3
;;
(ert-deftest shu-test-shu-cunsplit-3 ()
  "Doc string."
  (let ((data "\"A\"\n\"B\"")
        (expected "\"AB\"")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cunsplit-4
;;
(ert-deftest shu-test-shu-cunsplit-4 ()
  "Doc string."
  (let ((data "\"Now is the \"\n\"time for all \"\n\"good men \"\n\"to come to the aid \"\n\"of the party\"\n")
        (expected "\"Now is the time for all good men to come to the aid of the party\"\n")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 7)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cunsplit-5
;;
(ert-deftest shu-test-shu-cunsplit-5 ()
  "Doc string."
  (let ((data "\"Now is the \"\n\"time for all \"\n\"good men \"\n\"to come to the aid \"\n\"of the party\"\n")
        (expected "\"Now is the time for all good men to come to the aid of the party\"\n")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 24)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cunsplit-6
;;
(ert-deftest shu-test-shu-cunsplit-6 ()
  "Doc string."
  (let ((data "\"Now is the \"\n\"time for all \"\n\"good men \"\n\"to come to the aid \"\n\"of the party\"\n")
        (expected "\"Now is the time for all good men to come to the aid of the party\"\n")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 34)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cunsplit-6
;;
(ert-deftest shu-test-shu-cunsplit-6 ()
  "Doc string."
  (let ((data "\"Now is the \"\n\"time for all \"\n\"good men \"\n\"to come to the aid \"\n\"of the party\"\n")
        (expected "\"Now is the time for all good men to come to the aid of the party\"\n")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 44)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cunsplit-7
;;
(ert-deftest shu-test-shu-cunsplit-7 ()
  "Doc string."
  (let ((data "\"Now is the \"\n\"time for all \"\n\"good men \"\n\"to come to the aid \"\n\"of the party\"\n")
        (expected "\"Now is the time for all good men to come to the aid of the party\"\n")
        (actual) )
    (with-temp-buffer
      (insert data)
      (goto-char 70)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-creplace-1
;;
(ert-deftest shu-test-shu-creplace-1 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (expected-replace1
      (concat
              "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lac\"\n"
              "\"inia lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida\"\n"
              "\" interdum et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor\"\n"
              "\". Etiam eget enim pharetra,tristique ex at, porta dui. Fusce varius non or\"\n"
              "\"ci ut semper. Nuncfinibus lorem at elit varius, volutpat semper arcuinterd\"\n"
              "\"um. Quisque egestas tristique velit vel varius. In nisinulla, mollis quis \"\n"
              "\"mauris sit amet, dictum molestiejusto. Curabitur feugiat eu mi at consecte\"\n"
              "\"tur. Sed ultrices massavel turpis pulvinar tristique. Etiam aliquam vulput\"\n"
              "\"ate magna,vitae commodo leo dictum at. Donec aliquam purus tortor, sit ame\"\n"
              "\"tvulputate orci facilisis at.\""))
    )
      ;; Do a shu-creplace of a split string with a long, unquoted string
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-csplit)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different string in the kill ring
          (insert replace1)
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 30)           ;; Go to first line of split string
      (shu-creplace)           ;; Replace with contents of kill ring
      ;; Buffer must hold the expected result
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
))



;;
;;  shu-test-shu-creplace-2
;;
(ert-deftest shu-test-shu-creplace-2 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (replace2)
    (expected-replace1
      (concat
              "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lac\"\n"
              "\"inia lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida\"\n"
              "\" interdum et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor\"\n"
              "\". Etiam eget enim pharetra,tristique ex at, porta dui. Fusce varius non or\"\n"
              "\"ci ut semper. Nuncfinibus lorem at elit varius, volutpat semper arcuinterd\"\n"
              "\"um. Quisque egestas tristique velit vel varius. In nisinulla, mollis quis \"\n"
              "\"mauris sit amet, dictum molestiejusto. Curabitur feugiat eu mi at consecte\"\n"
              "\"tur. Sed ultrices massavel turpis pulvinar tristique. Etiam aliquam vulput\"\n"
              "\"ate magna,vitae commodo leo dictum at. Donec aliquam purus tortor, sit ame\"\n"
              "\"tvulputate orci facilisis at.\""))
    )
  ;; Do a shu-creplace with a quoted string in the kill ring
  (setq replace2 (concat "\"" replace1 "\""))
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-csplit)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different, quoted string in the kill ring
          (insert replace2)    ;; in the kill ring
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)
      (shu-creplace)
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
))



;;
;;  shu-test-shu-creplace-3
;;
(ert-deftest shu-test-shu-creplace-3 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (replace2)
    )
  ;; Do a shu-creplace with a string in the kill ring that has a quote
  ;; at the beginning but not the end
  (setq replace2 (concat "\"" replace1))
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-csplit)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
          (insert replace2)    ;; has a quote at beginning but not end
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-creplace)           ;; Try to do a replace
      (should (= 10 (point)))  ;; Nothing should have happened
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      ;; Buffer should remain unchanged
      (should (string= shu-test-cpp-general-expected-split1 actual-replace)))
))



;;
;;  shu-test-shu-creplace-4
;;
(ert-deftest shu-test-shu-creplace-4 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (replace2)
    )
  ;; Do a shu-creplace with a string in the kill ring that has a quote
  ;; at the end but not the beginning
  (setq replace2 (concat  replace1 "\""))
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-csplit)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
          (insert replace2)    ;; has a quote at beginning but not end
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-creplace)           ;; Try to do a replace
      (should (= 10 (point)))  ;; Nothing should have happened
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      ;; Buffer should remain unchanged
      (should (string= shu-test-cpp-general-expected-split1 actual-replace)))

))



;;
;;  shu-test-shu-creplace-5
;;
(ert-deftest shu-test-shu-creplace-5 ()
  (let ((actual-split)
        (actual-replace)
        (original1
         (concat
          "\"Ut porta, quam eget tempor aliquet, lectus elit pulvinar dolor, sit amet d\"\n"
          "\"ignissim est massa ut arcu. Donec est dolor, ultricies eu cursus id, imper\"\n"
          "\"diet aliquam dui. Pellentesque ut blandit quam. Nunc dictum tempus enim no\"\n"
          "\"n elementum. Phasellus scelerisque purus sapien, quis congue ipsum ultrice\"\n"
          "\"s ut. Sed vel nibh ornare, sodales mi sed, pretium ex. Integer convallis, \"\n"
          "\"quam vulputate tempus volutpat, dui odio tincidunt nisi, et tincidunt nunc\"\n"
          "\" lectus id velit. Donec volutpat mi non laoreet scelerisque. Sed id leo si\"\n"
          "\"t amet mauris hendrerit ullamcorper. Curabitur fermentum libero vel ullamc\"\n"
          "\"orper feugiat. Nunc et hendrerit nulla, nec condimentum urna. Nullam et co\"\n"
          "\"ndimentum nisl, id semper ante. Vivamus eu tempor erat, sed tincidunt mi. \"\n"
          "\"Phasellus et massa viverra sapien bibendum tempor eget a enim. Duis varius\"\n"
          "\", dolor in ultrices posuere, lorem enim tincidunt enim, at iaculis libero \"\n"
          "\"eros id felis. Sed et justo mattis dolor porttitor fermentum id ut lorem.\""))
        (replace1
         (concat
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
          "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
          "tincidunt gravida interdum et, egestas quis dolor. Quisque"
          "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
          "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
          "finibus lorem at elit varius, volutpat semper arcu"
          "interdum. Quisque egestas tristique velit vel varius. In nisi"
          "nulla, mollis quis mauris sit amet, dictum molestie"
          "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
          "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
          "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
          "vulputate orci facilisis at."))
        (expected-replace1
         (concat
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lac\"\n"
          "\"inia lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida\"\n"
          "\" interdum et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor\"\n"
          "\". Etiam eget enim pharetra,tristique ex at, porta dui. Fusce varius non or\"\n"
          "\"ci ut semper. Nuncfinibus lorem at elit varius, volutpat semper arcuinterd\"\n"
          "\"um. Quisque egestas tristique velit vel varius. In nisinulla, mollis quis \"\n"
          "\"mauris sit amet, dictum molestiejusto. Curabitur feugiat eu mi at consecte\"\n"
          "\"tur. Sed ultrices massavel turpis pulvinar tristique. Etiam aliquam vulput\"\n"
          "\"ate magna,vitae commodo leo dictum at. Donec aliquam purus tortor, sit ame\"\n"
          "\"tvulputate orci facilisis at.\"")))
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (with-temp-buffer
      (insert original1)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 626)          ;; Go to five lines from the bottom
      (shu-creplace)           ;; Replace with contents of kill ring
      ;; Buffer must hold the expected result
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
    ))



;;
;;  shu-test-cif
;;
(ert-deftest shu-test-cif ()
  (let (
    (expected-if1  "if ()\n{\n}")
    (expected-if2  "         if ()\n         {\n         }")
    (actual-if )
       )
  (with-temp-buffer
      (shu-cif)
      (should (= 5 (point)))
      (setq actual-if (buffer-substring-no-properties 1 10))
      (should (string= expected-if1 actual-if))
      (goto-char (point-max))
      (insert "\n         ")
      (shu-cif)
      (should (= 24 (point)))
      (setq actual-if (buffer-substring-no-properties 11 (point-max)))
      (should (string= expected-if2 actual-if))
  )
))


;;
;;  shu-test-celse
;;
(ert-deftest shu-test-celse ()
  (let (
    (expected-else1  "else\n{\n    \n}\n")
    (expected-else2  "         else\n         {\n             \n         }\n")
    (actual-else )
       )
  (with-temp-buffer
      (shu-celse)
      (should (= 12 (point)))
      (setq actual-else (buffer-substring-no-properties 1 15))
      (should (string= expected-else1 actual-else))
      (goto-char (point-max))
      (insert "\n         ")
      (shu-celse)
      (should (= 54 (point)))
      (setq actual-else (buffer-substring-no-properties 16 (point-max)))
      (should (string= expected-else2 actual-else))
  )
))


;;
;;  shu-test-cfor
;;
(ert-deftest shu-test-cfor ()
  (let (
    (expected-for1  "for ()\n{\n}")
    (expected-for2  "         for ()\n         {\n         }")
    (actual-for )
       )
  (with-temp-buffer
      (shu-cfor)
      (should (= 6 (point)))
      (setq actual-for (buffer-substring-no-properties 1 11))
      (should (string= expected-for1 actual-for))
      (goto-char (point-max))
      (insert "\n         ")
      (shu-cfor)
      (should (= 26 (point)))
      (setq actual-for (buffer-substring-no-properties 12 (point-max)))
      (should (string= expected-for2 actual-for))
  )
))


;;
;;  shu-test-cwhile
;;
(ert-deftest shu-test-cwhile ()
  (let (
    (expected-while1  "while ()\n{\n}")
    (expected-while2  "         while ()\n         {\n         }")
    (actual-while )
       )
  (with-temp-buffer
      (shu-cwhile)
      (should (= 8 (point)))
      (setq actual-while (buffer-substring-no-properties 1 13))
      (should (string= expected-while1 actual-while))
      (goto-char (point-max))
      (insert "\n         ")
      (shu-cwhile)
      (should (= 30 (point)))
      (setq actual-while (buffer-substring-no-properties 14 (point-max)))
      (should (string= expected-while2 actual-while))
  )
))


;;
;;  shu-test-cdo
;;
(ert-deftest shu-test-cdo ()
  (let (
    (expected-do1  "do\n{\n} while();")
    (expected-do2  "         do\n         {\n         } while();")
    (actual-do )
       )
  (with-temp-buffer
      (shu-cdo)
      (should (= 14 (point)))
      (setq actual-do (buffer-substring-no-properties 1 16))
      (should (string= expected-do1 actual-do))
      (goto-char (point-max))
      (insert "\n         ")
      (shu-cdo)
      (should (= 57 (point)))
      (setq actual-do (buffer-substring-no-properties 17 (point-max)))
      (should (string= expected-do2 actual-do))
  )
))



;;
;;  shu-test-shu-qualify-class-name-1
;;
(ert-deftest shu-test-shu-qualify-class-name-1 ()
  "Add a namespace to a class name when it is the only thing in the buffer"
  (let* ((class "Mumble")
        (namespace "abcdef")
        (expected (concat namespace "::" class))
        (actual)
        (count))
    (with-temp-buffer
      (insert class)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-2
;;
(ert-deftest shu-test-shu-qualify-class-name-2 ()
  "Add a namespace to a class name when it is the only thing in the buffer and it has
something in front of it."
  (let* ((class "Mumble")
        (namespace "abcdef")
        (prefix "  <")
        (suffix "")
        (expected (concat prefix namespace "::" class suffix))
        (actual)
        (count))
    (with-temp-buffer
      (insert (concat prefix class suffix))
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-3
;;
(ert-deftest shu-test-shu-qualify-class-name-3 ()
  "Add a namespace to a class name when it is the only thing in the buffer and it has
something in front of it and following it."
  (let* ((class "Mumble")
        (namespace "abcdef")
        (prefix "  <")
        (suffix ">")
        (expected (concat prefix namespace "::" class suffix))
        (actual)
        (count))
    (with-temp-buffer
      (insert (concat prefix class suffix))
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-4
;;
(ert-deftest shu-test-shu-qualify-class-name-4 ()
  "Do not add a namespace to something that might look like a class name but has the
wrong case."
  (let* ((class "Mumble")
        (data "mumble")
        (namespace "abcdef")
        (prefix "  <")
        (suffix ">")
        (expected (concat prefix data suffix))
        (actual)
        (count))
    (with-temp-buffer
      (insert (concat prefix data suffix))
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-5
;;
(ert-deftest shu-test-shu-qualify-class-name-5 ()
  "Do not add a namespace to something that might look like a class name but is part
of a variable name with characters in front and with something also surrounding it."
  (let* ((class "Mumble")
        (data "d_Mumble")
        (namespace "abcdef")
        (prefix "  <")
        (suffix ">")
        (expected (concat prefix data suffix))
        (actual)
        (count))
    (with-temp-buffer
      (insert (concat prefix data suffix))
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-6
;;
(ert-deftest shu-test-shu-qualify-class-name-6 ()
  "Do not add a namespace to something that might look like a class name but is part
of a variable name with characters at the end and with something also surrounding it."
  (let* ((class "Mumble")
        (data "MumbleIn")
        (namespace "abcdef")
        (prefix "  <")
        (suffix ">")
        (expected (concat prefix data suffix))
        (actual)
        (count))
    (with-temp-buffer
      (insert (concat prefix data suffix))
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-7
;;
(ert-deftest shu-test-shu-qualify-class-name-7 ()
  "Do not add a namespace to something that might look like a class name but is part
of a variable name with characters in front."
  (let* ((class "Mumble")
        (data "d_Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-8
;;
(ert-deftest shu-test-shu-qualify-class-name-8 ()
  "Do not add a namespace to something that might look like a class name but is part
of a variable name with characters at the end."
  (let* ((class "Mumble")
        (data "MumbleIn")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-9
;;
(ert-deftest shu-test-shu-qualify-class-name-9 ()
  "Add a namespace to multiple class names."
  (let* ((class "Mumble")
        (data "Mumble Mumble    Mumble")
        (namespace "abcdef")
        (expected "abcdef::Mumble abcdef::Mumble    abcdef::Mumble")
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 3 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-10
;;
(ert-deftest shu-test-shu-qualify-class-name-10 ()
  "Do not add namespace to qualified class name."
  (let* ((class "Mumble")
        (data "abcdef::Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-11
;;
(ert-deftest shu-test-shu-qualify-class-name-11 ()
  "Do not add namespace to qualified class name."
  (let* ((class "Mumble")
        (data "abcdef  ::  Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-12
;;
(ert-deftest shu-test-shu-qualify-class-name-12 ()
  "Do not add namespace to qualified class name."
  (let* ((class "Mumble")
        (data "abcdef::Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-13
;;
(ert-deftest shu-test-shu-qualify-class-name-13 ()
  "Do not add namespace to lower case name."
  (let* ((class "Mumble")
        (data "mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-14
;;
(ert-deftest shu-test-shu-qualify-class-name-14 ()
  "Do not add namespace to class name preceeded by \">\".  This is most likely
an arrow operator preceeding a function call."
  (let* ((class "Mumble")
        (data "boo -> Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-15
;;
(ert-deftest shu-test-shu-qualify-class-name-15 ()
  "Do not add namespace to class name preceeded by \">\".  This is most likely
an arrow operator preceeding a function call."
  (let* ((class "Mumble")
        (data "boo->Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-16
;;
(ert-deftest shu-test-shu-qualify-class-name-16 ()
  "Do not add namespace to class name preceeded by \".\".  This is most likely
a reference doing a function call."
  (let* ((class "Mumble")
        (data "boo.Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-17
;;
(ert-deftest shu-test-shu-qualify-class-name-17 ()
  "Do not add namespace to class name preceeded by \".\".  This is most likely
a reference doing a function call."
  (let* ((class "Mumble")
        (data "boo . Mumble")
        (namespace "abcdef")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-18
;;
(ert-deftest shu-test-shu-qualify-class-name-18 ()
  "Do not reject a class name that is preceeded by a dot, arrow, or colon on
a previous line."
  (let* ((class "Mumble")
        (data "boo . \n  Mumble")
        (namespace "abcdef")
        (expected "abcdef::Mumble")
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties 10 (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-19
;;
(ert-deftest shu-test-shu-qualify-class-name-19 ()
  "Do not add namespace to class name preceeded by \"#include\" on the same line.
This is most likely the name of an include file and not the name of a class."
  (let* ((class "string")
        (data "#include <string>")
        (namespace "std")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-20
;;
(ert-deftest shu-test-shu-qualify-class-name-20 ()
  "Do not add namespace to class name preceeded by \"#include\" on the same line.
This is most likely the name of an include file and not the name of a class."
  (let* ((class "string")
        (data " # include <string>")
        (namespace "std")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-21
;;
(ert-deftest shu-test-shu-qualify-class-name-21 ()
  "Do not add namespace to class name that is locaated inside a string."
  (let* ((class "set")
        (data "\"This is how we set the pointer.\"")
        (namespace "std")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-22
;;
(ert-deftest shu-test-shu-qualify-class-name-22 ()
  "Do not add namespace to class name that is locaated inside a comment."
  (let* ((class "set")
        (data " // This is how we set the pointer.")
        (namespace "std")
        (expected data)
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-23
;;
(ert-deftest shu-test-shu-qualify-class-name-23 ()
  "Add a namespace to a class name that is followed by a comment."
  (let* ((class "Mumble")
        (namespace "abcdef")
        (prefix "  ")
        (suffix "  // A comment")
        (expected (concat prefix namespace "::" class suffix))
        (data (concat prefix class suffix))
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))


;;
;;  shu-test-shu-qualify-class-name-24
;;
(ert-deftest shu-test-shu-qualify-class-name-24 ()
  "Add a namespace to a class name that is preceeded by a comment in a string."
  (let* ((class "Mumble")
        (namespace "abcdef")
        (prefix "  \" // cmt in string \" ")
        (suffix "  ")
        (expected (concat prefix namespace "::" class suffix))
        (data (concat prefix class suffix))
        (actual)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq count (shu-qualify-class-name class namespace))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
))




;;
;;  shu-test-shu-cpp-qualify-classes-1
;;
(ert-deftest shu-test-shu-cpp-qualify-classes-1 ()
  "Call with a list of classes and a single namespace, no buffer."
  (let* ((classes (list "map" "set" "string" "vector"))
         (namespace "std")
         (data "map set")
         (expected "std::map std::set")
         (actual)
         (count))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-qualify-classes classes namespace))
      (should (= 2 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-qualify-classes-2
;;
(ert-deftest shu-test-shu-cpp-qualify-classes-2 ()
  "Call with a list of classes and a list of namespaces, no buffer."
  (let* ((classes   (list "map" "set" "string" "vector"))
         (namespace (list "std" "std"    "std"    "std"))
         (data "map set")
         (expected "std::map std::set")
         (actual)
         (count))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-qualify-classes classes namespace))
      (should (= 2 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-qualify-classes-3
;;
(ert-deftest shu-test-shu-cpp-qualify-classes-3 ()
  "Call with a list of classes and a single namespace and a buffer."
  (let* ((buf (get-buffer-create shu-unit-test-buffer))
         (classes (list "map" "set" "string" "vector"))
         (namespace "std")
         (data "map set")
         (expected "std::map std::set")
         (actual)
         (count))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-qualify-classes classes namespace buf))
      (should (= 2 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-qualify-classes-4
;;
(ert-deftest shu-test-shu-cpp-qualify-classes-4 ()
  "Call with a list of classes and a list of namespaces, no buffer."
  (let* ((buf (get-buffer-create shu-unit-test-buffer))
         (classes   (list "map" "set" "string" "vector"))
         (namespace (list "std" "std"    "std"    "std"))
         (data "map set")
         (expected "std::map std::set")
         (actual)
         (count))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-qualify-classes classes namespace buf))
      (should (= 2 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-1
;;
(ert-deftest shu-test-shu-cpp-rmv-using-1 ()
  (let ((data
         (concat
          "#include <something.h>\n"
          "using namespace std;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   vector<string>   q;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"  (list "string" "set" "map" "vector"))))
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   std::vector<std::string>   q;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 4 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-2
;;
(ert-deftest shu-test-shu-cpp-rmv-using-2 ()
  (let ((data
         (concat
          "#include <something.h>\n"
          "using namespace WhammoCorp::std;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   vector<string>   q;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"  (list "string" "set" "map" "vector"))))
        (top-name "WhammoCorp")
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   std::vector<std::string>   q;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes top-name))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 4 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-3
;;
(ert-deftest shu-test-shu-cpp-rmv-using-3 ()
  (let ((data
         (concat
          "#include <something.h>\n"
          "using namespace std;\n"
          "using namespace world;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   Hello     q;\n"
          "   vector<string>   q;\n"
          "   Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "world" (list "Hello" "Goodbye"))))
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   world::Hello     q;\n"
          "   std::vector<std::string>   q;\n"
          "   world::Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 6 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-4
;;
(ert-deftest shu-test-shu-cpp-rmv-using-4 ()
  (let ((data
         (concat
          "#include <something.h>\n"
          "using namespace std;\n"
          "using namespace able;\n"
          "using namespace world;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   Hello     q;\n"
          "   vector<string>   q;\n"
          "   Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "world" (list "Hello" "Goodbye"))))
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "using namespace able;\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   world::Hello     q;\n"
          "   std::vector<std::string>   q;\n"
          "   world::Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 6 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-5
;;
(ert-deftest shu-test-shu-cpp-rmv-using-5 ()
  "Doc string."
  (let* ((data
          (concat
           "#include <something.h>\n"
           "using namespace WhammoCorp::std;\n"
           "using namespace bsl;\n"
           "   string    x;\n"
           "   set<int>  y;\n"
           "   vector<string>   q;\n"
           "   z->set();\n"
           "// vector<string> \n"))
         (classes
          (list
           (cons "std"  (list "string" "set" "map" "vector"))
           (cons "bsl"  (list "string" "set"))))
         (top-name "WhammoCorp")
         (expected data)
         (actual)
         (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes top-name))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 0 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-6
;;
(ert-deftest shu-test-shu-cpp-rmv-using-6 ()
  (let ((data
         (concat
          "#include <something.h>\n"
          "using namespace std;\n"
          "// using namespace muddle;\n"
          "using namespace world;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   Hello     q;\n"
          "   vector<string>   q;\n"
          "   Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "muddle"   (list "Whirlwind"))
          (cons "world" (list "Hello" "Goodbye"))))
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "// using namespace muddle;\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   world::Hello     q;\n"
          "   std::vector<std::string>   q;\n"
          "   world::Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 6 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-7
;;
(ert-deftest shu-test-shu-cpp-rmv-using-7 ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (data
         (concat
          "#include <something.h>\n"
          "using namespace std;\n"
          "using namespace world;\n"
          "using namespace std;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   Hello     q;\n"
          "   vector<string>   q;\n"
          "   Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "world" (list "Hello" "Goodbye"))))
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   world::Hello     q;\n"
          "   std::vector<std::string>   q;\n"
          "   world::Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (princ (concat "\nexpected:\n" expected "\n") gb)
      (princ (concat "\nactual:\n" actual "\n") gb)
      (should (string= expected actual)))
    (should (= 6 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-8
;;
(ert-deftest shu-test-shu-cpp-rmv-using-8 ()
  (let ((data
         (concat
          "#include <something.h>\n"
          "using namespace std;\n"
          "\" using namespace muddle; \"\n"
          "using namespace world;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   Hello     q;\n"
          "   vector<string>   q;\n"
          "   Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "muddle"   (list "Whirlwind"))
          (cons "world" (list "Hello" "Goodbye"))))
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "\" using namespace muddle; \"\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   world::Hello     q;\n"
          "   std::vector<std::string>   q;\n"
          "   world::Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (setq debug-on-error t)
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 6 count))
    ))



;;
;;  shu-test-shu-cpp-rmv-using-9
;;
(ert-deftest shu-test-shu-cpp-rmv-using-9 ()
  (let ((data
         (concat
          "#include <something.h>\n"
          "using namespace std;\n"
          "\" using namespace muddle; \"\n"
          "using namespace world;\n"
          "using namespace std;\n"
          "   string    x;\n"
          "   set<int>  y;\n"
          "   Hello     q;\n"
          "   vector<string>   q;\n"
          "   Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "muddle"   (list "Whirlwind"))
          (cons "world" (list "Hello" "Goodbye"))))
        (expected
         (concat
          "#include <something.h>\n"
          "\n"
          "\" using namespace muddle; \"\n"
          "\n"
          "\n"
          "   std::string    x;\n"
          "   std::set<int>  y;\n"
          "   world::Hello     q;\n"
          "   std::vector<std::string>   q;\n"
          "   world::Goodbye  g;\n"
          "   Goodbyebye  bb;\n"
          "   z->set();\n"
          "// vector<string> \n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-cpp-rmv-using-old classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 6 count))
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
;;  shu-test-shu-binclude-1
;;
(ert-deftest shu-test-shu-binclude-1 ()
  (let* ((data "  abcdef::MumbleFrotz  x(5);\n")
        (left-delim (if shu-cpp-include-user-brackets "<" "\""))
        (right-delim (if shu-cpp-include-user-brackets ">" "\""))
        (actual)
        (expected
         (concat
          "#include "
          left-delim "abcdef_mumblefrotz.h" right-delim)))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "Mum"))
      (shu-binclude))
    (with-temp-buffer
      (yank)
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-binclude-2
;;
(ert-deftest shu-test-shu-binclude-2 ()
  (let* ((data "  abcdef::MumbleFrotz  x(5);\n")
        (left-delim (if shu-cpp-include-user-brackets "<" "\""))
        (right-delim (if shu-cpp-include-user-brackets ">" "\""))
        (actual)
        (expected
         (concat
          "#include "
          left-delim "abcdef_mumblefrotz.h" right-delim)))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "abc"))
      (shu-binclude))
    (with-temp-buffer
      (yank)
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-binclude-3
;;
(ert-deftest shu-test-shu-binclude-3 ()
  (let* ((data "  abcdef::MumbleFrotz  x(5);\n")
        (left-delim (if shu-cpp-include-user-brackets "<" "\""))
        (right-delim (if shu-cpp-include-user-brackets ">" "\""))
        (actual)
        (expected
         (concat
          "#include "
          left-delim "abcdef_mumblefrotz.h" right-delim)))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward ":"))
      (shu-binclude))
    (with-temp-buffer
      (yank)
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-binclude-4
;;
(ert-deftest shu-test-shu-binclude-4 ()
  (let* ((data "  abcdef::MumbleFrotz xxx(5);\n")
        (left-delim (if shu-cpp-include-user-brackets "<" "\""))
        (right-delim (if shu-cpp-include-user-brackets ">" "\"")))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "xxx"))
      (should (not (shu-binclude))))
    ))




;;
;;  shu-test-shu-cpp-fix-prototype-1
;;
(ert-deftest shu-test-shu-cpp-fix-prototype-1 ()
  (let* ((pad (make-string shu-cpp-indent-length ? ))
         (data
          (concat
           "double Frobnitz::hitRatio(\n"
           "    const int  reads,\n"
           "    const int  writes)\n"
           "const\n"))
         (expected
          (concat
           pad "double hitRatio(\n"
           pad "    const int  reads,\n"
           pad "    const int  writes)\n"
           pad "const;\n"))
         (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-cpp-fix-prototype)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-fix-prototype-2
;;
(ert-deftest shu-test-shu-cpp-fix-prototype-2 ()
  (let* ((pad (make-string shu-cpp-indent-length ? ))
         (data
          (concat
           "double Frobnitz::hitRatio(\n"
           "    const int  reads,\n"
           "    const int  writes)\n"
           "\n"))
         (expected
          (concat
           pad "double hitRatio(\n"
           pad "    const int  reads,\n"
           pad "    const int  writes);\n"
           "\n"))
         (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-cpp-fix-prototype)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-fix-prototype-3
;;
;;(ert-deftest shu-test-shu-cpp-fix-prototype-3 ()
;;  (let* ((pad (make-string shu-cpp-indent-length ? ))
;;         (data
;;          (concat
;;           "const MetricsMap &MetricsCollection::metrics() const\n"
;;           "\n"))
;;         (expected
;;          (concat
;;           pad "const MetricsMap &metrics() const;\n"
;;           "\n"))
;;         (actual))
;;    (with-temp-buffer
;;      (insert data)
;;      (goto-char (point-min))
;;      (shu-cpp-fix-prototype)
;;      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
;;      (should actual)
;;      (should (stringp actual))
;;      (should (string= expected actual)))
;;    ))
;;



;;
;;  shu-test-shu-cpp-get-variable-name-1
;;
(ert-deftest shu-test-shu-cpp-get-variable-name-1 ()
  (let* ((expected-name "mumble")
         (actual-name)
        (data
         (concat
          "\n  // hello\n"
          "x = " expected-name  " * 2\n")))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward expected-name nil t))
      (backward-char 2)
      (setq actual-name (shu-cpp-get-variable-name))
      (should actual-name)
      (should (stringp actual-name))
      (should (string= expected-name actual-name)))
    ))



;;
;;  shu-test-shu-cpp-get-variable-name-2
;;
(ert-deftest shu-test-shu-cpp-get-variable-name-2 ()
  (let* ((expected-name "mumble")
         (actual-name)
        (data expected-name))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward expected-name nil t))
      (backward-char 2)
      (setq actual-name (shu-cpp-get-variable-name))
      (should actual-name)
      (should (stringp actual-name))
      (should (string= expected-name actual-name)))
    ))


;;
;;  shu-test-shu-cpp-get-variable-name-3
;;
(ert-deftest shu-test-shu-cpp-get-variable-name-3 ()
  (let* ((non-name "@.@!")
         (actual-name)
        (data
         (concat
          "\n  // hello\n"
          "x = " non-name  " * 2\n")))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward non-name nil t))
      (backward-char 2)
      (setq actual-name (shu-cpp-get-variable-name))
      (should (not actual-name)))
    ))



;;
;;  shu-test-shu-cpp-get-variable-name-position-1
;;
(ert-deftest shu-test-shu-cpp-get-variable-name-position-1 ()
  (let ((data "  mumbleFrotz  ")
        (ret-val)
        (expected (cons 3 14)))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "mum" nil t))
      (setq ret-val (shu-cpp-get-variable-name-position))
      (should ret-val)
      (should (consp ret-val))
      (should (equal expected ret-val)))
    ))



;;
;;  shu-test-shu-cpp-get-variable-name-position-2
;;
(ert-deftest shu-test-shu-cpp-get-variable-name-position-2 ()
  (let ((data "         ")
        (ret-val))
    (with-temp-buffer
      (insert data)
      (goto-char 5)
      (setq ret-val (shu-cpp-get-variable-name-position))
      (should (not ret-val)))
    ))




;;
;;  shu-test-shu-citerate
;;
(ert-deftest shu-test-shu-citerate ()
  (let* ((type-name "std::vector<Thing *>")
         (var-name "things")
         (pad "    ")
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           "    for (std::vector<Thing *>::iterator it = things.begin();\n"
           "         it != things.end(); ++it)\n"
           "    {\n"
           "    " ipad "\n"
           "    }\n"
           ))
         (actual))
    (with-temp-buffer
      (insert pad)
      (shu-citerate type-name var-name)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cciterate
;;
(ert-deftest shu-test-shu-cciterate ()
  (let* ((type-name "std::vector<Thing *>")
         (var-name "things")
         (pad "    ")
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           "    for (std::vector<Thing *>::const_iterator it = things.begin();\n"
           "         it != things.end(); ++it)\n"
           "    {\n"
           "    " ipad "\n"
           "    }\n"
           ))
         (actual))
    (with-temp-buffer
      (insert pad)
      (shu-cciterate type-name var-name)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-diterate
;;
(ert-deftest shu-test-shu-diterate ()
  (let* ((type-name "std::vector<Thing *>")
         (var-name-1 "lhs")
         (var-name-2 "rhs")
         (pad "    ")
         (ipad (make-string shu-cpp-indent-length ? ))
         (pair-name (concat shu-cpp-std-namespace "::pair"))
         (expected
          (concat
           "    for (" pair-name "<std::vector<Thing *>::iterator,\n"
           "                   std::vector<Thing *>::iterator>\n"
           "             its(lhs.begin(), rhs.begin());\n"
           "         its.first != lhs.end() && its.second != rhs.end();\n"
           "         ++its.first, ++its.second)\n"
           "    {\n"
           "    " ipad "\n"
           "    }\n"
           ))
         (actual))
    (with-temp-buffer
      (insert pad)
      (shu-diterate type-name var-name-1 var-name-2)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-dciterate
;;
(ert-deftest shu-test-shu-dciterate ()
  (let* ((type-name "std::vector<Thing *>")
         (var-name-1 "lhs")
         (var-name-2 "rhs")
         (pad "    ")
         (ipad (make-string shu-cpp-indent-length ? ))
         (pair-name (concat shu-cpp-std-namespace "::pair"))
         (expected
          (concat
           "    for (" pair-name "<std::vector<Thing *>::const_iterator,\n"
           "                   std::vector<Thing *>::const_iterator>\n"
           "             its(lhs.begin(), rhs.begin());\n"
           "         its.first != lhs.end() && its.second != rhs.end();\n"
           "         ++its.first, ++its.second)\n"
           "    {\n"
           "    " ipad "\n"
           "    }\n"
           ))
         (actual))
    (with-temp-buffer
      (insert pad)
      (shu-dciterate type-name var-name-1 var-name-2)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-titerate
;;
(ert-deftest shu-test-shu-titerate ()
  (let* ((type-name-1 "std::set<Thing *>")
         (type-name-2 "std::vector<ThingBob *>")
         (var-name-1 "things")
         (var-name-2 "bobs")
         (pad "    ")
         (ipad (make-string shu-cpp-indent-length ? ))
         (pair-name (concat shu-cpp-std-namespace "::pair"))
         (expected
          (concat
           "    for (" pair-name "<std::set<Thing *>::iterator,\n"
           "                   std::vector<ThingBob *>::iterator>\n"
           "             its(things.begin(), bobs.begin());\n"
           "         its.first != things.end() && its.second != bobs.end();\n"
           "         ++its.first, ++its.second)\n"
           "    {\n"
           "    " ipad "\n"
           "    }\n"
           ))
         (actual))
    (with-temp-buffer
      (insert pad)
      (shu-titerate type-name-1 type-name-2 var-name-1 var-name-2)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-tciterate
;;
(ert-deftest shu-test-shu-tciterate ()
  (let* ((type-name-1 "std::set<Thing *>")
         (type-name-2 "std::vector<ThingBob *>")
         (var-name-1 "things")
         (var-name-2 "bobs")
         (pad "    ")
         (ipad (make-string shu-cpp-indent-length ? ))
         (pair-name (concat shu-cpp-std-namespace "::pair"))
         (expected
          (concat
           "    for (" pair-name "<std::set<Thing *>::const_iterator,\n"
           "                   std::vector<ThingBob *>::const_iterator>\n"
           "             its(things.begin(), bobs.begin());\n"
           "         its.first != things.end() && its.second != bobs.end();\n"
           "         ++its.first, ++its.second)\n"
           "    {\n"
           "    " ipad "\n"
           "    }\n"
           ))
         (actual))
    (with-temp-buffer
      (insert pad)
      (shu-tciterate type-name-1 type-name-2 var-name-1 var-name-2)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))





;;; shu-cpp-general.t.el ends here
