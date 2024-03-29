;;; neogen-tests.el --- Unit tests for neogen.el -*- lexical-binding: t -*-

(require 'buttercup)
(require 'tree-sitter)
(require 'neogen)
(require 'neogen-lang)
(require 'cc-mode)

(defmacro neogen-with-test-file (mode-cmd contents &rest body)
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,contents)
     (funcall ,mode-cmd)
     (tree-sitter-mode)
     (goto-char (point-min))
     (yas-minor-mode)
     ,@body))

(describe "neogen-c"
  (before-each
    (spy-on 'file-name-extension :and-return-value "c"))

  (it "Function generation works"
    (neogen-with-test-file #'c-mode "#include <stdio.h>

int main() {
   printf(\"Hello, World!\");
   return 0;
}"
      (search-forward "int main()")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp "/\\*\\*\n \\* @brief .*\n \\*\n \\* @return .*\n \\*/"))))

  (it "Function generation works with parameters"
    (neogen-with-test-file #'c-mode "int add(int a, int b) {
  printf(\"not doing anythign...\")
}
"
      (search-forward "int add(")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " * @brief " (0+ not-newline) "\n"
                                         " *\n"
                                         " * @param a " (0+ not-newline) "\n"
                                         " * @param b " (0+ not-newline) "\n"
                                         " * @return" (0+ not-newline) "\n"
                                         " */")))))
  
  (it "Void function shouldn't have return statement"
    (neogen-with-test-file #'c-mode "
void noreturn(int a, int b) {
  printf(\"do nothing\")
}
"
      (search-forward "void noreturn(")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " * @brief " (0+ not-newline) "\n"
                                         " *\n"
                                         " * @param a " (0+ not-newline) "\n"
                                         " * @param b " (0+ not-newline) "\n"
                                         " */")))))
  (it "correctly annotates a typedef"
    (neogen-with-test-file #'c-mode "
#include <stdio.h>

typedef unsigned char BYTE;
"
      (search-forward "unsigned")
      (neogen-type)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " * @typedef BYTE\n" 
                                         " * @brief" (0+ not-newline) "\n"
                                         " *\n"
                                         " */")))))
  (it "correctly annotates a file"
    (neogen-with-test-file #'c-mode "
#include <stdio.h>

typedef unsigned char BYTE;
"
      (neogen-file)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " * @file\n" 
                                         " * @brief" (0+ not-newline) "\n"
                                         " */"))))))

(describe "neogen-js"
  (before-each
    (spy-on 'file-name-extension :and-return-value "js"))
  (it "Function generation works"
    (neogen-with-test-file #'js-mode "
function hello(abc, def) {
  return false
}
"
      (search-forward "hello(")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " * @param {any} abc" (0+ not-newline) "\n"
                                         " * @param {any} def" (0+ not-newline) "\n"
                                         " * @returns {}" (0+ not-newline) "\n"
                                         " */")))))

  (it "Class generation works"
    (neogen-with-test-file #'js-mode "
class Rectangle2 {
  constructor(height, width) {
    this.height = height;
    this.width = width;
  }
}
"
      (search-forward "Rectangle")
      (neogen-class)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " *" (0+ not-newline) "\n"
                                         " * @class" (0+ not-newline) "\n"
                                         " * @classdesc" (0+ not-newline) "\n"
                                         " */")))))
  (it "Type annotation works"
    (neogen-with-test-file #'js-mode "
function alert(a) {
  return a;
}

var funny;
"
      (search-forward "funny")
      (neogen-type)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/* @type" (0+ not-newline) "*/"))))))


(describe "neogen-ts"
  (before-each
    (spy-on 'file-name-extension :and-return-value "ts"))
  (it "Function generation works"
    (neogen-with-test-file #'typescript-mode "
function hello(abc: number, def:number) {
  return false
}
"
      (search-forward "hello(")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " * @param {number} abc" (0+ not-newline) "\n"
                                         " * @param {number} def" (0+ not-newline) "\n"
                                         " * @returns {}" (0+ not-newline) "\n"
                                         " */")))))
  (it "Type generation works"
    (neogen-with-test-file #'typescript-mode "
var funny;
"
      (search-forward "funny")
      (neogen-type)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/* @type" (0+ not-newline) "*/")))))
  (it "Class generation works"
    (neogen-with-test-file #'typescript-mode "
class Rectangle2 {
  constructor(height, width) {
    this.height=height
  }
}
"
      (search-forward "Rectangle")
      (neogen-class)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " *" (0+ not-newline) "\n"
                                         " * @class" (0+ not-newline) "\n"
                                         " * @classdesc" (0+ not-newline) "\n"
                                         " */"))))))

(describe "neogen-sh"
  (before-each
    (spy-on 'file-name-extension :and-return-value "sh"))
  (it "Function generation works"
    (neogen-with-test-file #'sh-mode "
function my_function() {
   echo \"${abc} ${ndef}\"
   echo \"${abc} $def $ghi\"
}
"
      (search-forward "my_function(")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "#######################################\n"
                                         "#" (0+ not-newline) "\n"
                                         "# Globals:\n"
                                         "#   abc" (0+ not-newline) "\n"
                                         "#   ndef" (0+ not-newline) "\n"
                                         "#   abc" (0+ not-newline) "\n"
                                         "# Arguments:\n"
                                         "#" (0+ not-newline) "\n"
                                         "#" (0+ not-newline) "\n"
                                         "#######################################\n")))))
  (it "File generation works"
    (neogen-with-test-file #'sh-mode
        "
function my_function() {
   echo \"${abc} ${ndef}\"
   echo \"${abc} $def $ghi\"
}
"
      (search-forward "my_function(")
      (neogen-file)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "#!/bin/bash"))))))



(provide 'neogen-tests)

;;; neogen-tests.el ends here
