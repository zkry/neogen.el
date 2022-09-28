# neogen.el - Emacs port of Neogen

## Features

Neogen.el is an Emacs port of https://github.com/danymat/neogen, a
package for generating annotation templates.  The package uses
Tree-sitter to extract the code context and is able to fill in
standard documentation templates (ex. jsdoc or doxygen).

## Installation

This package is currently a work-in-progress and has not been added to
MELPA.

## Usage and Configuration

Your buffer should first have `tree-sitter-mode` enabled
(https://github.com/emacs-tree-sitter/elisp-tree-sitter).

You can generate a template via the command `neogen`, which will
prompt you for a template type.  The available template types are:
`func`, `class`, `type`, and `file`.  Note that not all languages
support all types.  Convenience commands `neogen-func`,
`neogen-class`, `neogen-type`, and `neogen-file` are also provided.

Assuming neogen has a corresponding template type and can find the
correct position in the tree, it will generate and write a Yasnippet
template, which allows you to fill in the values and tab through the
various insertion positions.

The following is a table of configurations and templates:

| Language   | Annotation Convention | Supported annotation types |
|------------|-----------------------|----------------------------|
| C          | Doxygen               | func, file, type           |
| javascript | JSDoc                 | func, class, type, file    |

If you don't see your desired language/annotation-syntax supported
feel free to open up an issue.
