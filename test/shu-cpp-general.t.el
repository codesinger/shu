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
   "\"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNO\"\n"
   "\"PQRSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghijklmnopqrstuvwxyz1234\"\n"
   "\"567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ\"")
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
    (setq shu-cpp-line-end 80)
    (setq debug-on-error t)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move to char right after quote
      (shu-internal-csplit t)             ;; Now try to split
      (should (= 215 (point))) ;; Point should have moved
      ;; Make sure the result is what we expect
      ;;      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
      ;;      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      ;;      (goto-char 20)
      ;;      ;; This should restore it to its original state
      ;;      (shu-cunsplit)
      ;;      (setq actual-unsplit (buffer-substring-no-properties 1 (1+ (length shu-test-cpp-general-base-string))))
      ;;      (should (string= shu-test-cpp-general-base-string actual-unsplit))
      )
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
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert base3)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 2)         ;; Move 2 chars right of quote
      (shu-internal-csplit t)             ;; Try to split again
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
         (concat "      \"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHI\"\n"
                 "      \"JKLMNOPQRSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghijklmnopqr\"\n"
                 "      \"stuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ\""))
        )
    ;; Split of one long line with 6 blanks in front of
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert (concat "      " shu-test-cpp-general-base-string))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-internal-csplit t)           ;; Now try to split
      (should (= 233 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 233))
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
  (let ((gb (get-buffer-create "**foo**"))
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
         (concat
          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "      \"\\t\\t\\t\\t\"")))

    ;; Split a long string full of tab characters
    ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert (concat "      " base2))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-internal-csplit t)           ;; Now try to split
      (should (= 179 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 179))
      (should (string= expected-split3 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 7 (+ 7 (length base2))))
      (should (string= base2 actual-unsplit)))
    ))



;;
;;  shu-test-shu-csplit-4a
;;
(ert-deftest shu-test-shu-csplit-4a ()
  (let ((actual-split)
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
         (concat
          ;;          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          ;;          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          ;;          "      \"\\t\\t\\t\\t\""))
          ;;          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          ;;          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          ;;          "       \"\\t\\t\\t\\t\\t\\t\""))
          ;;
          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "       \"\\t\\t\\t\\t\\t\\t\""))
        (z)
        )

    ;; Split a long string full of tab characters
    ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert (concat "       " base2))
      (setq z (buffer-substring-no-properties (point-min) (point-max)))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-internal-csplit t)           ;; Now try to split
      (should (= 182 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 182))
      (should (string= expected-split3 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 8 (+ 8 (length base2))))
      ;;      (setq actual-unsplit (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= base2 actual-unsplit)))
    ))




;;
;;  shu-test-shu-csplit-5
;;
(ert-deftest shu-test-shu-csplit-5 ()
  (let ((actual-split)
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
          "\n")))
    (with-temp-buffer
      (insert fail-case-unexpected)
      (goto-char (point-min))
      (search-forward "Lorem" nil t)
      (shu-cunsplit)
      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= fail-case-unexpected-after-split actual-split)))
    ))



;;
;;  shu-test-shu-csplit-6
;;
(ert-deftest shu-test-shu-csplit-6 ()
  (let ((actual-split)
        (actual-unsplit)
        (base2
         (concat
          "\"Whan that Aprille with his shoures soote "
          "The droghte of Marche hath perced to the roote, "
          "And bathed every veyne in swich licour, "
          "Of which vertu engendred is the flour; "
          "Whan Zephirus eek with his swete breeth "
          "Inspired hath in every holt and heeth "
          "The tendre croppes, and the yonge sonne "
          "Hath in the Ram his halfe cours y-ronne, "
          "And smale fowles maken melodye, "
          "That slepen al the night with open ye, "
          "(So priketh hem nature in hir corages: "
          "Than longen folk to goon on pilgrimages,\""
          ))
        (expected-split4
         (concat
          "      \"Whan that Aprille with his shoures soote The droghte of Marche hath \"\n"
          "      \"perced to the roote, And bathed every veyne in swich licour, Of which \"\n"
          "      \"vertu engendred is the flour; Whan Zephirus eek with his swete breeth \"\n"
          "      \"Inspired hath in every holt and heeth The tendre croppes, and the yonge \"\n"
          "      \"sonne Hath in the Ram his halfe cours y-ronne, And smale fowles maken \"\n"
          "      \"melodye, That slepen al the night with open ye, (So priketh hem nature \"\n"
          "      \"in hir corages: Than longen folk to goon on pilgrimages,\""
          ))
        (z))

    ;; Split a long string full of tab characters
    ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert (concat "      " base2))
      (setq z (buffer-substring-no-properties (point-min) (point-max)))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)    ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (goto-char (point-min))
      (should (search-forward "that" nil t))
      (shu-internal-csplit)    ;; Now try to split
      ;;  (should (= 179 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-split4 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 7 (+ 7 (length base2))))
      (should (string= base2 actual-unsplit)))
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
          "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
          "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
          "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
          "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
          "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
          "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
          "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
          "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\""))
        )
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 30)           ;; Go to first line of split string
      (shu-internal-creplace t)           ;; Replace with contents of kill ring
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
          "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
          "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
          "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
          "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
          "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
          "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
          "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
          "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\""))
        )
    ;; Do a shu-creplace with a quoted string in the kill ring
    (setq replace2 (concat "\"" replace1 "\""))
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different, quoted string in the kill ring
        (insert replace2)    ;; in the kill ring
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)
      (shu-internal-creplace t)
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
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
        (insert replace2)    ;; has a quote at beginning but not end
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-internal-creplace t)           ;; Try to do a replace
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
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
        (insert replace2)    ;; has a quote at beginning but not end
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-internal-creplace t)           ;; Try to do a replace
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
          "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
          "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
          "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
          "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
          "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
          "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
          "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
          "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\"")))
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert original1)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 626)          ;; Go to five lines from the bottom
      (shu-creplace t)           ;; Replace with contents of kill ring
      ;; Buffer must hold the expected result
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
    ))



;;
;;  shu-test-shu-creplace-6
;;
(ert-deftest shu-test-shu-creplace-6 ()
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto \"\n"
          "\"lacinia lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida \"\n"
          "\"interdum et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. \"\n"
          "\"Etiam eget enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut \"\n"
          "\"semper. Nuncfinibus lorem at elit varius, volutpat semper arcuinterdum. \"\n"
          "\"Quisque egestas tristique velit vel varius. In nisinulla, mollis quis mauris \"\n"
          "\"sit amet, dictum molestiejusto. Curabitur feugiat eu mi at consectetur. Sed \"\n"
          "\"ultrices massavel turpis pulvinar tristique. Etiam aliquam vulputate \"\n"
          "\"magna,vitae commodo leo dictum at. Donec aliquam purus tortor, sit \"\n"
          "\"ametvulputate orci facilisis at.\"")))
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert original1)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 626)          ;; Go to five lines from the bottom
      (shu-internal-creplace)           ;; Replace with contents of kill ring
      ;; Buffer must hold the expected result
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
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
;;  shu-test-shu-cunsplit-7
;;
(ert-deftest shu-test-shu-cunsplit-7 ()
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
;;  shu-test-shu-cunsplit-8
;;
(ert-deftest shu-test-shu-cunsplit-8 ()
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
;;  shu-test-shu-cunsplit-9
;;
(ert-deftest shu-test-shu-cunsplit-9 ()
  (let ((data
         (concat
          "const std::string  expected(\"[../abcdef_ghijklmnopqrstu.vwx:...] <yzaBc\"\n"
          "                            \"::HijklmNopqrstuvwxyZab': Cdefgh: \\\"123456\\\"\"\n"
          "                            \"\\\".  Ijk lm nopqr stu 'vwxyza::BcdefgHijkl\"\n"
          "                            \"mnopqrsTuv': Wxyzab: \\\"654321\\\" cdefghi jk\"\n"
          "                            \"lmno pq rst uvwx yzabcdefghij Kl.\");"))
        (expected
         (concat
          "const std::string  expected(\"[../abcdef_ghijklmnopqrstu.vwx:...] "
          "<yzaBc::HijklmNopqrstuvwxyZab': Cdefgh: \\\"123456\\\"\\\".  Ijk "
          "lm nopqr stu 'vwxyza::BcdefgHijklmnopqrsTuv': Wxyzab: \\\"654321\\\" "
          "cdefghi jklmno pq rst uvwx yzabcdefghij Kl.\");"
          ))
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (search-forward "expected" nil t)
      (forward-char 9)
      (shu-cunsplit)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should actual)
      (should (stringp actual))
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
          "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
          "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
          "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
          "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
          "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
          "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
          "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
          "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\""))
        )
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 30)           ;; Go to first line of split string
      (shu-internal-creplace t)           ;; Replace with contents of kill ring
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
          "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
          "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
          "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
          "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
          "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
          "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
          "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
          "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\""))
        )
    ;; Do a shu-creplace with a quoted string in the kill ring
    (setq replace2 (concat "\"" replace1 "\""))
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different, quoted string in the kill ring
        (insert replace2)    ;; in the kill ring
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)
      (shu-internal-creplace t)
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
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
        (insert replace2)    ;; has a quote at beginning but not end
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-internal-creplace t)           ;; Try to do a replace
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
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
        (insert replace2)    ;; has a quote at beginning but not end
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-internal-creplace t)           ;; Try to do a replace
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
          "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
          "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
          "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
          "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
          "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
          "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
          "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
          "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\"")))
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert original1)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 626)          ;; Go to five lines from the bottom
      (shu-creplace t)           ;; Replace with contents of kill ring
      ;; Buffer must hold the expected result
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
    ))



;;
;;  shu-test-shu-creplace-6
;;
(ert-deftest shu-test-shu-creplace-6 ()
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
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto \"\n"
          "\"lacinia lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida \"\n"
          "\"interdum et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. \"\n"
          "\"Etiam eget enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut \"\n"
          "\"semper. Nuncfinibus lorem at elit varius, volutpat semper arcuinterdum. \"\n"
          "\"Quisque egestas tristique velit vel varius. In nisinulla, mollis quis mauris \"\n"
          "\"sit amet, dictum molestiejusto. Curabitur feugiat eu mi at consectetur. Sed \"\n"
          "\"ultrices massavel turpis pulvinar tristique. Etiam aliquam vulputate \"\n"
          "\"magna,vitae commodo leo dictum at. Donec aliquam purus tortor, sit \"\n"
          "\"ametvulputate orci facilisis at.\"")))
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert original1)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 626)          ;; Go to five lines from the bottom
      (shu-internal-creplace)           ;; Replace with contents of kill ring
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
  (let ((data
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
;;  shu-test-shu-binclude-5
;;
(ert-deftest shu-test-shu-binclude-5 ()
  (let* ((namespace (if shu-cpp-use-bde-library "bsl" "std"))
         (data (concat "  " namespace "::size_t  x(5);\n"))
         (left-delim (if shu-cpp-include-user-brackets "<" "\"")
                     )
         (right-delim (if shu-cpp-include-user-brackets ">" "\"")
                      )
         (actual)
         (hname (if shu-cpp-use-bde-library "bsl_cstddef.h" "cstddef"))
         (expected
          (concat
           "#include "
           left-delim hname right-delim)))
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



;;
;;  shu-test-shu-to-camel-1
;;
(ert-deftest shu-test-shu-to-camel-1 ()
  (let ((data "mumble_something_other")
        (expected "mumbleSomethingOther")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char 4)
      (shu-to-camel)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))


;;
;;  shu-test-shu-to-camel-2
;;
(ert-deftest shu-test-shu-to-camel-2 ()
  (let ((data "   mumble_something_other   ")
        (expected "   mumbleSomethingOther   ")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "mumble" nil t))
      (shu-to-camel)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))


;;
;;  shu-test-shu-to-camel-3
;;
(ert-deftest shu-test-shu-to-camel-3 ()
  (let ((data "   boo__hoo   ")
        (expected "   booHoo   ")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "boo" nil t))
      (shu-to-camel)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))


;;
;;  shu-test-shu-to-camel-4
;;
(ert-deftest shu-test-shu-to-camel-4 ()
  (let ((data "   boo_hoo_   ")
        (expected "   booHoo   ")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "boo" nil t))
      (shu-to-camel)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))


;;
;;  shu-test-shu-to-camel-5
;;
(ert-deftest shu-test-shu-to-camel-5 ()
  (let ((data "   _boo_hoo_   ")
        (expected "   BooHoo   ")
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "boo" nil t))
      (shu-to-camel)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-gcc-1
;;
(ert-deftest shu-test-shu-gcc-1 ()
  (let* ((expected "mumblebar with fruit on top")
         (data
          (concat
           "blither blather bother $ " expected "\n"
           "This is some random text\n"
           "This is more random stuff\n"
           "And yet more rubbish\n"
           "etc.\n"))
         (actual))
    (with-temp-buffer
      (insert data)
      (shu-gcc))
    (with-temp-buffer
      (yank)
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-map-class-to-include-1
;;
(ert-deftest shu-test-shu-cpp-map-class-to-include-1 ()
  (let* ((namespace (if shu-cpp-use-bde-library "bsl" "std"))
         (class-name (concat namespace "::make_shared"))
         (expected-name (if shu-cpp-use-bde-library "bsl_memory.h" "memory"))
         (actual-name))
    (setq actual-name (shu-cpp-map-class-to-include class-name))
    (should actual-name)
    (should (stringp actual-name))
    (should (string= expected-name actual-name))
    ))




;;
;;  shu-test-shu-cpp-map-class-to-include-2
;;
(ert-deftest shu-test-shu-cpp-map-class-to-include-2 ()
  (let ((class-name "zzz::mumble")
        (actual-name))
    (setq actual-name (shu-cpp-map-class-to-include class-name))
    (should (not actual-name))
    ))



;;
;;  shu-test-shu-std-include-list
;;
(ert-deftest shu-test-shu-std-include-list ()
  "Check to ensure that SHU-STD-INCLUDE-LIST does not include any overlapping
class names."
  (let ((ret-val)
        (ht)
        (dup-alist))
    (setq ret-val (shu-invert-alist-to-hash shu-std-include-list))
    (should ret-val)
    (should (consp ret-val))
    (setq ht (car ret-val))
    (should ht)
    (should (hash-table-p ht))
    (setq dup-alist (cdr ret-val))
    (should (not dup-alist))
    ))



;;
;;  shu-test-shu-bsl-include-list
;;
(ert-deftest shu-test-shu-bsl-include-list ()
  "Check to ensure that SHU-BSL-INCLUDE-LIST does not include any overlapping
class names."
  (let ((ret-val)
        (ht)
        (dup-alist))
    (setq ret-val (shu-invert-alist-to-hash shu-bsl-include-list))
    (should ret-val)
    (should (consp ret-val))
    (setq ht (car ret-val))
    (should ht)
    (should (hash-table-p ht))
    (setq dup-alist (cdr ret-val))
    (should (not dup-alist))
    ))



;;
;;  shu-test-shu-cpp-internal-fill-test-data-1
;;
(ert-deftest shu-test-shu-cpp-internal-fill-test-data-1 ()
  (let ((data
         (concat
          "    "
          shu-cpp-datetime-timezone-type
          "  abc;\n"
          ))
        (in-length)
        (out-length)
        (did-fill)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (setq did-fill (shu-cpp-internal-fill-test-data))
      (should did-fill)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 8))))
    ))



;;
;;  shu-test-shu-cpp-internal-fill-test-data-2
;;
(ert-deftest shu-test-shu-cpp-internal-fill-test-data-2 ()
  (let ((data
         (concat
          "    "
          shu-cpp-interval-type
          "  abc;\n"
          ))
        (in-length)
        (out-length)
        (did-fill)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (setq did-fill (shu-cpp-internal-fill-test-data))
      (should did-fill)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 8))))
    ))



;;
;;  shu-test-shu-cpp-internal-fill-test-data-3
;;
(ert-deftest shu-test-shu-cpp-internal-fill-test-data-3 ()
  (let ((data
         (concat
          "    "
          shu-cpp-datetime-type
          "  abc;\n"
          ))
        (in-length)
        (out-length)
        (did-fill)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (setq did-fill (shu-cpp-internal-fill-test-data))
      (should did-fill)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 8))))
    ))



;;
;;  shu-test-shu-cpp-internal-fill-test-data-4
;;
(ert-deftest shu-test-shu-cpp-internal-fill-test-data-4 ()
  (let ((data
         (concat
          "    "
          shu-cpp-size-type
          "  abc;\n"
          ))
        (in-length)
        (out-length)
        (did-fill)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (setq did-fill (shu-cpp-internal-fill-test-data))
      (should did-fill)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 8))))
    ))



;;
;;  shu-test-shu-cpp-internal-fill-test-data-5
;;
(ert-deftest shu-test-shu-cpp-internal-fill-test-data-5 ()
  (let ((data
         (concat
          "    "
          shu-cpp-string-type
          "  abc;\n"
          ))
        (in-length)
        (out-length)
        (did-fill)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (setq did-fill (shu-cpp-internal-fill-test-data))
      (should did-fill)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 8))))
    ))



;;
;;  shu-test-shu-cpp-internal-fill-test-data-6
;;
(ert-deftest shu-test-shu-cpp-internal-fill-test-data-6 ()
  (let ((data
         (concat
          "    const "
          shu-cpp-string-type
          "  abc;\n"
          ))
        (in-length)
        (out-length)
        (did-fill)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char 6)
      (setq did-fill (shu-cpp-internal-fill-test-data))
      (should did-fill)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 8))))
    ))



;;
;;  shu-test-shu-cpp-fill-test-area-1
;;
(ert-deftest shu-test-shu-cpp-fill-test-area-1 ()
  (let ((data
         (concat
          "    " shu-cpp-datetime-timezone-type "  ab;\n"
          "    " shu-cpp-interval-type          "  cd;\n"
          "    " shu-cpp-datetime-type          "  ef;\n"
          "    " shu-cpp-size-type              "  gh;\n"
          "    " shu-cpp-string-type            "  ij;\n"
          "    " shu-cpp-time-type              "  kl;\n"
          "    " shu-cpp-long-long-type         "  mn;\n"
          "    " "bool"                         "  op;\n"
          "    " "char"                         "  qr;\n"
          "    " "double"                       "  st;\n"
          "    " "float"                        "  uv;\n"
          "    " "int"                          "  wx;\n"
          "    " "short"                        "  yz;\n"
          ))
        (result)
        (skip-count)
        (fill-count)
        (in-length)
        (out-length)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq result (shu-cpp-fill-test-area (point-min) (point-max)))
      (should result)
      (should (consp result))
      (setq skip-count (car result))
      (setq fill-count (cdr result))
      (should skip-count)
      (should (numberp skip-count))
      (should (= skip-count 0))
      (should (numberp fill-count))
      (should (= fill-count 13))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 62))))
  ))



;;
;;  shu-test-shu-cpp-fill-test-area-2
;;
(ert-deftest shu-test-shu-cpp-fill-test-area-2 ()
  (let ((data
         (concat
          "    const " shu-cpp-datetime-timezone-type "  ab;\n"
          "    const " shu-cpp-interval-type          "  cd;\n"
          "    const " shu-cpp-datetime-type          "  ef;\n"
          "    const " shu-cpp-size-type              "  gh;\n"
          "    const " shu-cpp-string-type            "  ij;\n"
          "    const " shu-cpp-time-type              "  kl;\n"
          "    const " shu-cpp-long-long-type         "  mn;\n"
          "    const " "bool"                         "  op;\n"
          "    const " "char"                         "  qr;\n"
          "    const " "double"                       "  st;\n"
          "    const " "float"                        "  uv;\n"
          "    const " "int"                          "  wx;\n"
          "    const " "short"                        "  yz;\n"
          ))
        (result)
        (skip-count)
        (fill-count)
        (in-length)
        (out-length)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq result (shu-cpp-fill-test-area (point-min) (point-max)))
      (should result)
      (should (consp result))
      (setq skip-count (car result))
      (setq fill-count (cdr result))
      (should skip-count)
      (should (numberp skip-count))
      (should (= skip-count 0))
      (should (numberp fill-count))
      (should (= fill-count 13))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 62))))
  ))



;;
;;  shu-test-shu-cpp-fill-test-area-3
;;
(ert-deftest shu-test-shu-cpp-fill-test-area-3 ()
  (let ((data
         (concat
          "    " shu-cpp-datetime-timezone-type "  ab;\n"
          "    " shu-cpp-interval-type          "  cd;\n"
          "    " shu-cpp-datetime-type          "  ef;\n"
          "    " shu-cpp-size-type              "  gh;\n"
          "    " "mumblefrotz"                  "  aaa;\n"
          "    " shu-cpp-string-type            "  ij;\n"
          "    " shu-cpp-time-type              "  kl;\n"
          "    " shu-cpp-long-long-type         "  mn;\n"
          "    " "bool"                         "  op;\n"
          "    " "char"                         "  qr;\n"
          "    " "double"                       "  st;\n"
          "    " "mumblebars"                   "  bbb;\n"
          "    " "float"                        "  uv;\n"
          "    " "int"                          "  wx;\n"
          "    " "short"                        "  yz;\n"
          ))
        (result)
        (skip-count)
        (fill-count)
        (in-length)
        (out-length)
        (actual))
    (setq in-length (length data))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq result (shu-cpp-fill-test-area (point-min) (point-max)))
      (should result)
      (should (consp result))
      (setq skip-count (car result))
      (setq fill-count (cdr result))
      (should skip-count)
      (should (numberp skip-count))
      (should (= skip-count 2))
      (should (numberp fill-count))
      (should (= fill-count 13))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (setq out-length (length actual))
      (should (> out-length (+ in-length 62))))
  ))



;;
;;  shu-test-shu-sort-includes-1
;;
(ert-deftest shu-test-shu-sort-includes-1 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <gg>\n"
          "#include <aa>\n"
          "#include <zz>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <aa>\n"
          "#include <gg>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 5 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-2
;;
(ert-deftest shu-test-shu-sort-includes-2 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <gg>\n"
          "#include <aa>\n"
          "#include <zz>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <aa>\n"
          "#include <gg>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 5 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-3
;;
(ert-deftest shu-test-shu-sort-includes-3 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <gg>\n"
          "#include <aa>\n"
          "#include <zz>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <aa>\n"
          "#include <gg>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 5 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-4
;;
(ert-deftest shu-test-shu-sort-includes-4 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <gg>\n"
          "#include <aa>\n"
          "#include <zz>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <aa>\n"
          "#include <gg>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 5 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-5
;;
(ert-deftest shu-test-shu-sort-includes-5 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <gg>\n"
          "#include <aa>\n"
          "#include <zz>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <aa>\n"
          "#include <gg>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 5 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-6
;;
(ert-deftest shu-test-shu-sort-includes-6 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <gg6>\n"
          "#include <aa6>\n"
          "#  include <zz6>\n"
          "#include <rr6>\n"
          "#include <vv6>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <aa6>\n"
          "#include <gg6>\n"
          "#include <rr6>\n"
          "#include <vv6>\n"
          "#include <zz6>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 5 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-7
;;
(ert-deftest shu-test-shu-sort-includes-7 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-8
;;
(ert-deftest shu-test-shu-sort-includes-8 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <gg>\n"
          "#include <aa>\n"
          "#include <zz>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "#include <aa>\n"
          "#include <gg>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <aa>\n"
          "#include <gg>\n"
          "#include <rr>\n"
          "#include <vv>\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "include" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 5 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sort-includes-9
;;
(ert-deftest shu-test-shu-sort-includes-9 ()
  (let (
        (data
         (concat
          "// Hello\n"
          "// How are you?\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "// How are you?\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count)
          )
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "Hello" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-shu-sort-includes-10
;;
(ert-deftest shu-test-shu-sort-includes-10 ()
  (let (
        (data
         (concat
          "// Hello\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <zz>\n"
          "// Goodbye\n"))
        (actual)
        (found)
        (count)
          )
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "// He" nil t))
      (should found)
      (setq count (shu-sort-includes))
      (should count)
      (should (numberp count))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-shu-sort-all-includes-1
;;
(ert-deftest shu-test-shu-sort-all-includes-1 ()
  (let ((data
         (concat
          "// Hello\n"
          "#include <z1>\n"
          "#include <c1>\n"
          "#include <a1>\n"
          "// Goodbye\n"
          "#include <z2>\n"
          "#include <c2>\n"
          "#include <a2>\n"
          "// Again\n"))
        (expected
         (concat
          "// Hello\n"
          "#include <a1>\n"
          "#include <c1>\n"
          "#include <z1>\n"
          "// Goodbye\n"
          "#include <a2>\n"
          "#include <c2>\n"
          "#include <z2>\n"
          "// Again\n"))
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-sort-all-includes)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-find-include-blocks-1
;;
(ert-deftest shu-test-shu-cpp-find-include-blocks-1 ()
  (let ((data
         (concat
          "#include <a>\n"
          "#include <b>\n"
          "#include <c>\n"
          "\\ Hello\n"
          "#include <d>\n"
          "#include <e>\n"
          "#include <f>\n"
          "\\ Bye\n"))
        (pllist)
        (pl)
        (spoint)
        (epoint))
    (with-temp-buffer
      (insert data)
      (setq pllist (shu-cpp-find-include-blocks))
      (should pllist)
      (should (listp pllist))
      (should (= 2 (length pllist)))

      (setq pl (car pllist))
      (should pl)
      (should (consp pl))
      (setq spoint (car pl))
      (setq epoint (cdr pl))
      (should spoint)
      (should epoint)
      (should (numberp spoint))
      (should (numberp epoint))
      (should (= spoint 48))
      (should (= epoint 86))

      (setq pllist (cdr pllist))
      (setq pl (car pllist))
      (should pl)
      (should (consp pl))
      (setq spoint (car pl))
      (setq epoint (cdr pl))
      (should spoint)
      (should epoint)
      (should (numberp spoint))
      (should (numberp epoint))
      (should (= spoint 1))
      (should (= epoint 39)))
    ))


;;
;;  shu-test-shu-cpp-find-include-blocks-2
;;
(ert-deftest shu-test-shu-cpp-find-include-blocks-2 ()
  (let ((data
         (concat
          "#include <a>\n"
          "#include <b>\n"
          "\\ Hello\n"
          "#include <c>\n"
          "\\ Bye\n"
          "#include <d>\n"
          "#include <e>\n"
          "#include <f>\n"
          "\\ Bye\n"))
        (pllist)
        (pl)
        (spoint)
        (epoint))
    (with-temp-buffer
      (insert data)
      (setq pllist (shu-cpp-find-include-blocks))
      (should pllist)
      (should (listp pllist))
      (should (= 3 (length pllist)))

      (setq pl (car pllist))
      (should pl)
      (should (consp pl))
      (setq spoint (car pl))
      (setq epoint (cdr pl))
      (should spoint)
      (should epoint)
      (should (numberp spoint))
      (should (numberp epoint))
      (should (= spoint 54))
      (should (= epoint 92))

      (setq pllist (cdr pllist))
      (setq pl (car pllist))
      (should pl)
      (should (consp pl))
      (setq spoint (car pl))
      (setq epoint (cdr pl))
      (should spoint)
      (should epoint)
      (should (numberp spoint))
      (should (numberp epoint))
      (should (= spoint 35))
      (should (= epoint 47))

      (setq pllist (cdr pllist))
      (setq pl (car pllist))
      (should pl)
      (should (consp pl))
      (setq spoint (car pl))
      (setq epoint (cdr pl))
      (should spoint)
      (should epoint)
      (should (numberp spoint))
      (should (numberp epoint))
      (should (= spoint 1))
      (should (= epoint 26)))
    ))


;;
;;  shu-test-shu-cpp-find-include-blocks-3
;;
(ert-deftest shu-test-shu-cpp-find-include-blocks-3 ()
  (let ((data
         (concat
          "\\ Hello\n"
          "#include <c>\n"
          "\\ Bye\n"))
        (pllist)
        (pl)
        (spoint)
        (epoint))
    (with-temp-buffer
      (insert data)
      (setq pllist (shu-cpp-find-include-blocks))
      (should pllist)
      (should (listp pllist))
      (should (= 1 (length pllist)))

      (setq pl (car pllist))
      (should pl)
      (should (consp pl))
      (setq spoint (car pl))
      (setq epoint (cdr pl))
      (should spoint)
      (should epoint)
      (should (numberp spoint))
      (should (numberp epoint))
      (should (= spoint 9))
      (should (= epoint 21)))
    ))


;;
;;  shu-test-shu-cpp-find-include-blocks-4
;;
(ert-deftest shu-test-shu-cpp-find-include-blocks-4 ()
  (let ((data
         (concat
          "\\ Hello\n"
          "\\ Bye\n"))
        (pllist))
    (with-temp-buffer
      (insert data)
      (setq pllist (shu-cpp-find-include-blocks))
      (should (not pllist)))
    ))



;;
;;  shu-test-shu-cpp-find-include-locations-1
;;
(ert-deftest shu-test-shu-cpp-find-include-locations-1 ()
  (let ((data
         (concat
          "// Hello\n"
          "# include <mumble>\n"
          "#include <bob>\n"
          "// Goodbye\n"
          "#include <bumble>\n"))
        (pllist)
        (pl)
        (spoint)
        (line))
    (with-temp-buffer
      (insert data)
      (setq pllist (shu-cpp-find-include-locations)))
    (should pllist)
    (should (listp pllist))
    (should (= 3 (length pllist)))
    (setq pl (car pllist))
    (should pl)
    (should (consp pl))
    (setq spoint (car pl))
    (should spoint)
    (should (numberp spoint))
    (should (= 55 spoint))
    (setq line (cdr pl))
    (should line)
    (should (numberp line))
    (should (= 5 line))

    (setq pllist (cdr pllist))
    (should pllist)
    (should (listp pllist))
    (setq pl (car pllist))
    (should pl)
    (should (consp pl))
    (setq spoint (car pl))
    (should spoint)
    (should (numberp spoint))
    (should (= 29 spoint))
    (setq line (cdr pl))
    (should line)
    (should (numberp line))
    (should (= 3 line))

    (setq pllist (cdr pllist))
    (should pllist)
    (should (listp pllist))
    (setq pl (car pllist))
    (should pl)
    (should (consp pl))
    (setq spoint (car pl))
    (should spoint)
    (should (numberp spoint))
    (should (= 10 spoint))
    (setq line (cdr pl))
    (should line)
    (should (numberp line))
    (should (= 2 line))
    ))





;;
;;  shu-test-shu-cpp-find-include-locations-2
;;
(ert-deftest shu-test-shu-cpp-find-include-locations-2 ()
  (let ((data
         (concat
          "// Hello\n"
          "// Goodbye\n"))
        (pllist))
    (with-temp-buffer
      (insert data)
      (setq pllist (shu-cpp-find-include-locations)))
    (should (not pllist))
    ))


;;
;;  shu-test-shu-cpp-find-include-direction-1
;;
(ert-deftest shu-test-shu-cpp-find-include-direction-1 ()
  (let ((pllist
         (list
          (cons 15 2)
          (cons 30 4)))
        (actual)
        (expected 1))
    (setq actual (shu-cpp-find-include-direction pllist))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-shu-cpp-find-include-direction-2
;;
(ert-deftest shu-test-shu-cpp-find-include-direction-2 ()
  (let ((pllist
         (list
          (cons 30 4)
          (cons 15 2)))
        (actual)
        (expected -1))
    (setq actual (shu-cpp-find-include-direction pllist))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-shu-cpp-find-include-direction-3
;;
(ert-deftest shu-test-shu-cpp-find-include-direction-3 ()
  (let ((pllist
         (list
          (cons 30 4)))
        (actual)
        (expected 1))
    (setq actual (shu-cpp-find-include-direction pllist))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-shu-cpp-find-include-direction-4
;;
(ert-deftest shu-test-shu-cpp-find-include-direction-4 ()
  (let ((pllist)
        (actual)
        (expected 1))
    (setq actual (shu-cpp-find-include-direction pllist))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-1
;;
(ert-deftest shu-test-shu-make-sort-announcement-1 ()
  (let ((ret-val (cons 0 0))
        (group-count)
        (actual)
        (expected "Sorted 0 lines"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-2
;;
(ert-deftest shu-test-shu-make-sort-announcement-2 ()
  (let ((ret-val (cons 1 0))
        (group-count)
        (actual)
        (expected "Sorted 1 line"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-3
;;
(ert-deftest shu-test-shu-make-sort-announcement-3 ()
  (let ((ret-val (cons 2 0))
        (group-count)
        (actual)
        (expected "Sorted 2 lines"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-4
;;
(ert-deftest shu-test-shu-make-sort-announcement-4 ()
  (let ((ret-val (cons 0 1))
        (group-count)
        (actual)
        (expected "Sorted 0 lines (after deleting 1 duplicate)"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-5
;;
(ert-deftest shu-test-shu-make-sort-announcement-5 ()
  (let ((ret-val (cons 1 2))
        (group-count)
        (actual)
        (expected "Sorted 1 line (after deleting 2 duplicates)"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;
;;  shu-test-shu-make-sort-announcement-6
;;
(ert-deftest shu-test-shu-make-sort-announcement-6 ()
  (let ((ret-val (cons 0 0))
        (group-count 0)
        (actual)
        (expected "Sorted 0 lines in 0 groups"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;
;;  shu-test-shu-make-sort-announcement-7
;;
(ert-deftest shu-test-shu-make-sort-announcement-7 ()
  (let ((ret-val (cons 0 0))
        (group-count 1)
        (actual)
        (expected "Sorted 0 lines in 1 group"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-8
;;
(ert-deftest shu-test-shu-make-sort-announcement-8 ()
  (let ((ret-val (cons 1 0))
        (group-count 1)
        (actual)
        (expected "Sorted 1 line in 1 group"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-9
;;
(ert-deftest shu-test-shu-make-sort-announcement-9 ()
  (let ((ret-val (cons 2 0))
        (group-count 2)
        (actual)
        (expected "Sorted 2 lines in 2 groups"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-10
;;
(ert-deftest shu-test-shu-make-sort-announcement-10 ()
  (let ((ret-val (cons 0 1))
        (group-count 2)
        (actual)
        (expected "Sorted 0 lines in 2 groups (after deleting 1 duplicate)"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-sort-announcement-11
;;
(ert-deftest shu-test-shu-make-sort-announcement-11 ()
  (let ((ret-val (cons 1 2))
        (group-count 2)
        (actual)
        (expected "Sorted 1 line in 2 groups (after deleting 2 duplicates)"))
    (setq actual (shu-make-sort-announcement ret-val group-count))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-is-keyword-1
;;
(ert-deftest shu-test-shu-cpp-is-keyword-1 ()
    (should (shu-cpp-is-keyword "register"))
    )


;;
;;  shu-test-shu-cpp-is-keyword-2
;;
(ert-deftest shu-test-shu-cpp-is-keyword-2 ()
    (should (shu-cpp-is-keyword "static_cast"))
    )


;;
;;  shu-test-shu-cpp-is-keyword-3
;;
(ert-deftest shu-test-shu-cpp-is-keyword-3 ()
    (should (not (shu-cpp-is-keyword "ztatic_cast")))
    )


;;
;;  shu-test-shu-cpp-sitting-on-keyword-1
;;
(ert-deftest shu-test-shu-cpp-sitting-on-keyword-1 ()
  (let ((data "  static_cast  ")
        (found)
        (actual)
        (expected "static_cast"))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "stat" nil t))
      (should found)
      (setq actual (shu-cpp-sitting-on-keyword))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))


;;
;;  shu-test-shu-cpp-sitting-on-keyword-2
;;
(ert-deftest shu-test-shu-cpp-sitting-on-keyword-2 ()
  (let ((data "  dynamic_cast  ")
        (found)
        (actual)
        (expected "dynamic_cast"))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "dyna" nil t))
      (should found)
      (setq actual (shu-cpp-sitting-on-keyword))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))


;;
;;  shu-test-shu-cpp-sitting-on-keyword-3
;;
(ert-deftest shu-test-shu-cpp-sitting-on-keyword-3 ()
  (let ((data "  static_bast  ")
        (found)
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "stat" nil t))
      (should found)
      (should (not actual))
      )
    ))

;;; shu-cpp-general.t.el ends here
