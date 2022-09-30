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

(provide 'neogen-tests)

;;; neogen-tests.el ends here
