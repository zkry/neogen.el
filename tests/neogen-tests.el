;;; neogen-tests.el --- Unit tests for neogen.el -*- lexical-binding: t -*-

(require 'buttercup)
(require 'tree-sitter)
(require 'neogen)
(require 'f)

(describe "neogen-c"
  (before-each
    (spy-on 'file-name-extension :and-return-value "c"))
  (it "Function generation works"
    (with-temp-buffer
      (insert (f-read "./example-files/test.c"))
      (c-mode)
      (tree-sitter-mode)
      (goto-char (point-min))
      (yas-minor-mode)
      (search-forward "int main()")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp "/\\*\\*\n \\* @brief .*\n \\*\n \\* @return .*\n \\*/"))))
  (it "Function generation works with parameters"
    (with-temp-buffer
      (insert (f-read "./example-files/test.c"))
      (c-mode)
      (tree-sitter-mode)
      (goto-char (point-min))
      (yas-minor-mode)
      (search-forward "int add(")
      (neogen-func)
      (goto-char (point-min))
      (expect (search-forward-regexp (rx "/**\n"
                                         " * @brief " (0+ not-newline) "\n"
                                         " *\n"
                                         " * @param a " (0+ not-newline) "\n"
                                         " * @param b " (0+ not-newline) "\n"
                                         " */"))))))

(provide 'neogen-tests)

;;; neogen-tests.el ends here
