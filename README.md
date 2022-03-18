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

You can generate a template via the command `neogen`, which will
prompt you for a template type.  The available template types are:
`func`, `class`, `type`, and `file`.  Note that not all languages
support all types.  Convenience commands `neogen-func`,
`neogen-class`, `neogen-type`, and `neogen-file` are also provided.

Assuming neogen has a corresponding template type and can find the
correct position in the tree, it will generate and write a Yasnippet
template, which allows you to fill in the values and tab through the
various insertion positions.
