




;;
;;  shu-test-shu-cpp-rmv-using-7
;;
(ert-deftest shu-test-shu-cpp-rmv-using-7 ()
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
      (setq count (shu-cpp-rmv-using classes))
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
      (setq count (shu-cpp-rmv-using classes))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (should (= 6 count))
    ))
