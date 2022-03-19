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

There are two aspects to every neogen.el setup: a configuration and a
template.  There usually is just one configuration per language.  It's
the configuration's job to extract the required data from the
tree-sitter parse tree.  This data is then fed into the template where
the annotation is generated.  Language-configuration-template pairings
are made via the `neogen-mode-configuration-alist`.  The following is
an example configuration of this variable.

```lisp
(defvar neogen-mode-configuration-alist
  '((sh-mode neogen-configuration-sh neogen-template-google-bash)
    (c-mode neogen-configuration-c neogen-template-doxygen)
    (ruby-mode neogen-configuration-ruby neogen-template-rdoc)
    (python-mode neogen-configuration-python neogen-template-numpydoc)
    (php-mode neogen-configuration-php neogen-template-phpdoc)
    (typescript-mode neogen-configuration-typescript neogen-template-jsdoc)))
```

The following is a table of configurations and templates:

| major-mode      | Configuration                     | Template                                                      |
|-----------------|-----------------------------------|---------------------------------------------------------------|
| sh-mode         | `neogen-configuration-sh`         | `neogen-template-google-bash`                                 |
| c-mode          | `neogen-configuration-c`          | `neogen-template-doxygen`                                     |
| ruby-mode       | `neogen-configuration-ruby`       | `neogen-template-rdoc`                                        |
| python-mode     | `neogen-configuration-python`     | `neogen-template-google-docstring` `neogen-template-numpydoc` |
| php-mode        | `neogen-configuration-php`        | `neogen-template-phpdoc`                                      |
| typescript-mode | `neogen-configuration-typescript` | `neogen-template-jsdoc`                                       |
| javascript-mode | `neogen-configuration-javascript` | `neogen-template-jsdoc`                                       |
| rust-mode       | `neogen-configuration-rust`       | `neogen-template-rustdoc`                                     |


If you don't see your desired language/annotation-syntax supported
feel free to open up an issue.
