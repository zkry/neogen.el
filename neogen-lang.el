;;; neogen-lang.el --- Language configurations and templates. -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains configurations and templates of for various
;; tree-sitter grammers, used to generate documentaiton.

;;; Code:


;;; JavaScript/TypeScript

(defconst neogen-configuration-javascript--tree-query
  [(formal_parameters (identifier) @parameters)
   (statement_block (return_statement) @return_statement)])

(defconst neogen-configuration-javascript
  (neogen-config-create
   :func '(([method_definition function_declaration] [0]
            (lambda (node)
              (let* ((query (tsc-make-query tree-sitter-language neogen-configuration-javascript--tree-query))
                     (tree (tsc-query-captures query node #'ts--buffer-substring-no-properties)))
                tree)))
           ([expression_statement variable_declaration] [0]
            (lambda (node)
              (let* ((query (tsc-make-query tree-sitter-language
                                            (vector `(function ,(aref neogen-configuration-javascript--tree-query 0)
                                                               ,(aref neogen-configuration-javascript--tree-query 1)))))
                     (tree (tsc-query-captures query node #'ts--buffer-substring-no-properties)))
                tree)))
           ([lexical_declaration] [1]
            (lambda (node)
              (let* ((query (tsc-make-query tree-sitter-language
                                            (vector `(arrow_function ,(aref neogen-configuration-javascript--tree-query 0)
                                                                     ,(aref neogen-configuration-javascript--tree-query 1)))))
                     (tree (tsc-query-captures query node #'ts--buffer-substring-no-properties)))
                tree))))
   :class '(([function_declaration class_declaration expression_statement variable_declaration] [0]
             (lambda (node)
               '((class_name . ())))))
   :file '(([program] [0]
            (lambda (node)
              '())))
   :type '(([variable_declaration lexical_declaration] [0]
            (lambda (node)
              '())))))

(defconst neogen-configuration-typescript--function-tree
  '((:retrieve first
               :node-type formal_parameters
               :subtree ((:retrieve all
                                    :node-type required_parameter
                                    :extract t
                                    :as tparam)
                         (:retrieve all
                                    :node-type optional_parameter
                                    :extract t
                                    :as tparam)))
    (:retrieve first
               :node-type statement_block
               :subtree ((:retrieve first
                                    :node-type return_statement
                                    :extract t
                                    :as return_statement)))
    (:retrieve first
               :node-type type_annotation
               :subtree ((:retrieve first
                                    :node-type return_statement
                                    :extract t
                                    :as return_statement)))
    (:retrieve first
               :node-type type_parameters
               :subtree ((:retrieve all
                                    :node-type type_parameter
                                    :extract t
                                    :as type)))))

(defun neogen-configuration-typescript--construct-type-annotation (parameters)
  (when parameters
    (let ((subtree '((:retrieve all :node-type identifier :extract t :as parameters)
                     (:retrieve all :node-type predefined_type :recursive t :extract t :as type)
                     (:retrieve all :node-type type_identifier :recursive t :extract t :as type)))
          (results '()))
      (seq-map
       (lambda (param)
         (neogen-tree-query param subtree))
       parameters))))

(defconst neogen-configuration-typescript
  (neogen-config-create
   :func '(([function_declaration method_definition function_signature] [0]
            (lambda (node)
              (let ((res (neogen-tree-query node neogen-configuration-typescript--function-tree)))
                (setf (alist-get 'tparam res) (neogen-configuration-typescript--construct-type-annotation (alist-get 'tparam res)))
                (when (alist-get 'return_statement res)
                  (setf (alist-get 'return_statement res) t))
                res)))
           ([expression_statement variable_declaration] [0]
            (lambda (node)
              (let* ((tree `(:retrieve all :node-type function :subtree ,neogen-configuration-typescript--function-tree))
                     (res (neogen-tree-query node tree)))
                ;; (setf (alist-get 'tparam res) (neogen-configuration-typescript--construct-type-annotation (alist-get 'tparam res)))
                ;; (when (alist-get 'return_statement res)
                ;;   (setf (alist-get 'return_statement res) t))
                res)))
           ([lexical_declaration] [0]
            (lambda (node)
              (let* ((tree `(:retrieve all :node-type arrow_function :subtree ,neogen-configuration-typescript--function-tree))
                     (res (neogen-tree-query node tree)))
                ;; (setf (alist-get 'tparam res) (neogen-configuration-typescript--construct-type-annotation (alist-get 'tparam res)))
                ;; (when (alist-get 'return_statement res)
                ;;   (setf (alist-get 'return_statement res) t))
                res))))
   :class '(([function_declaration class_declaration expression_statement variable_declaration export_statement] [0]
             (lambda (node)
               '((class_name . "")))))
   :type '(([variable_declaration lexical_declaration] [0]
            (lambda (node)
              (let ((tree '(:retrieve first :node-type identifier :extract t :as type)))
                (neogen-tree-query node tree)))))
   :file '(([program] [0]
            (lambda (node)
              '())))))


;;; Go

(defconst neogen-configuration-go
  (neogen-config-create
   :func '(([function_declaration] [0]
            (lambda (node)
              '())))
   :type '(([package_clause const_declaration var_declaration] [0]
             (lambda (node)
               '())))))


;;; C/C++

(defconst neogen-configuration-c--c-params
  '(:node-type parameter_list
               :retrieve all
               :subtree ((:node-type [parameter_declaration]
                          :retrieve all
                          :subtree ((:retrieve first
                                     :recursive t
                                     :node-type identifier
                                     :extract t
                                     :as parameters))))))

(defun neogen-configuration-c-cpp-tree (is-cpp)
  (let ((tree-base `((:retrieve first
                                :node-type function_declarator
                                :subtree (,neogen-configuration-c--c-params))
                     (:retrieve first
                                :recursive true
                                :node-type function_declarator
                                :extract true)
                     (:retrieve first
                                :node-type compound_statement
                                :subtree ((:retrieve first
                                                     :node-type return_statement
                                                     :extract true)))
                     (:retrieve first
                                :node-type type_identifier
                                :extract t)
                     (:retrieve first
                                :node-type primitive_type
                                :extract t)
                     (:retrieve first
                                :node-type pointer_declarator
                                :extract t))))
    (when is-cpp
      (setq tree-base (cons '(:retrieve first
                                        :node-type template_parameter_list
                                        :subtree ((:retrieve first
                                                             :node-type [type_parameter_declaration variadic_type_parameter_declaration]
                                                             :subtree ((:retrieve all
                                                                                  :node-type type_identifier
                                                                                  :extract t)))))
                            tree-base)))
    tree-base))

(defconst neogen-configuration-c
  (neogen-config-create
   :func '(([function_declaration function_definition declaration field_declaration template_declaration] [0]
            (lambda (node)
              (let ((trees (neogen-configuration-c-cpp-tree nil)))
                (neogen-tree-query node trees)))))
   :file '(([translation_unit] [0]
            (lambda (node)
              '())))
   :type '(([type_definition] [1]
            (lambda (node)
              (let ((tree '(:node-type type_identifier
                                       :retrieve first
                                       :extract t
                                       :as type)))
                (neogen-tree-query node tree)))))))

(defconst neogen-configuration-cpp
  (neogen-config-create
   :func '(([function_declaration function_definition declaration field_declaration template_declaration] [0]
            (lambda (node)
              (let ((trees (neogen-configuration-c-cpp-tree t)))
                (neogen-tree-query node trees)))))
   :file '(([translation_unit] [0]
            (lambda (node)
              '())))
   :type '(([type_definition] [1]
            (lambda (node)
              (let ((tree '(:node-type type_identifier
                                       :retrieve first
                                       :extract t
                                       :as type)))
                (neogen-tree-query node tree)))))
   :class '(([class_specifier struct_specifier] [0]
            (lambda (node)
              (let ((tree '(:retrieve first
                                      :node-type type_identifier
                                      :extract t
                                      :as class_name)))
                (neogen-tree-query node tree)))))))


;;; Sh
(defconst neogen-configuration-sh
  (neogen-config-create
   :func '(([function_definition] [0]
            (lambda (node)
              (let ((tree '(:node-type compound_statement
                            :retrieve first
                            :subtree ((:node-type [simple_expansion]
                                       :retrieve all
                                       :recursive t
                                       :as parameters
                                       :extract t)
                                      (:node-type expansion
                                       :recursive t
                                       :retrieve all
                                       :subtree ((:node-type variable_name
                                                  :retrieve all
                                                  :extract t)))))))
                (neogen-tree-query node tree)))))
   :file '(([program] [0]
            (lambda (node)
              '())))))



;;; Ruby

(defconst neogen-configuration-ruby--identifier
  '(:retrieve first
    :node-type identifier
    :as parameters
    :extract t))

(defconst neogen-configuration-ruby
  (neogen-config-create
   :func '(([method] [0]
            (lambda (node)
              (let ((tree `((:retrieve first
                             :node-type method_parameters
                             :subtree ((:retrieve all
                                        :node-type optional_parameter
                                        :subtree (,neogen-configuration-ruby--identifier))
                                       ,neogen-configuration-ruby--identifier))
                            (:retrieve all
                             :node-type return
                             :as return_statement
                             :extract t))))
                (neogen-tree-query node tree)))))
   :class '(([class module] [0]
             (lambda (node)
               '())))
   :type '(([call] [0]
            (lambda (node)
              '())))))


;;; Rust

(defconst neogen-configuration-rust
  (neogen-config-create
   :func '(([function_item function_signature_item] [0]
            (lambda (node)
              (let ((tree '(:retrieve first
                            :node-type parameters
                            :subtree '((:retrieve all
                                        :node-type parameter
                                        :subtree '((:retrieve first
                                                    :node-type identifier
                                                    :extract true
                                                    :as parameters)))
                                       (:retrieve all
                                        :node-type type_identifier
                                        :extract t
                                        :as parameters)))))
                (neogen-tree-query node tree)))))
   :file '(([source_file] [0]
             (lambda (node)
               '())))
   :class '(([struct_item trait_item] [0]
             (lambda (node)
               (let ((tree '(:retrieve first
                             :node-type field_declaration_list
                             :subtree ((:retrieve all
                                        :node-type field-declaration
                                        :subtree ((:retrieve all
                                                   :node-type field-identifier
                                                   :extract t
                                                   :as parameters)))))))
                 (neogen-tree-query node tree)))))))


;;; Python

(defun neogen-configuration-python-func (node)
  (let* ((results '())
         (tree '((:retrieve all
                            :node-type parameters
                            :subtree ((:retrieve all
                                                 :top-level t
                                                 :node-type identifier
                                                 :extract t)
                                      (:retrieve all
                                                 :node-type default_parameter
                                                 :subtree ((:retrieve all
                                                                      :node-type identifier
                                                                      :extract t)))
                                      (:retrieve all
                                                 :node-type typed_parameter
                                                 :extract t)
                                      (:retrieve all
                                                 :node-type typed_default_parameter
                                                 :as typed_parameter
                                                 :extract t
                                                 :subtree ((:retrieve all
                                                                      :node-type identifier
                                                                      :extract t)))
                                      (:retrieve first
                                                 :node-type list_splat_pattern
                                                 :extract t
                                                 :as arbitrary_args)
                                      (:retrieve first
                                                 :node-type dictionary_splat_pattern
                                                 :extract t
                                                 :as arbitrary_args)))
                 (:retrieve first
                            :node-type block
                            :subtree ((:retrieve all
                                                 :node-type return_statement
                                                 :recursive t
                                                 :extract t)))
                 (:retrieve all
                            :node-type type
                            :as return_type_hint)))
         (nodes (neogen-tree-query node tree)))
    (when (assoc 'typed_parameter nodes)
      (let ((typed-parameters-result '()))
        (dolist (n (cdr (assoc 'typed_parameter nodes)))
          (let* ((type-subtree '((:retrieve all
                                            :node-type identifier
                                            :top-level t
                                            :extract t
                                            :as parameters)
                                 (:retrieve all
                                            :node-type type
                                            :extract t
                                            :as type)))
                 (res (neogen-tree-query n type-subtree)))
            (setq typed-parameters-result (append typed-parameters-result (list res)))))
        (setq results (cons (cons 'typed_parameters typed-parameters-result) nodes))))
    
    (when (assoc 'return_type_hint nodes)
      (setq nodes (assoc-delete-all 'return_statement node))
      (when (equal (tsc-node-text (cadr (assoc 'return_type_hint nodes)))
                   "None")
        (setq nodes (assoc-delete-all 'return_type_hint node))))

    ;; TODO Check if the function is inside a class
    ;;      If so, remove reference to the first parameter (that can be `self`, `cls`, or a custom name)
    ;;          see original implementation
    (setq results (append `((has_parameters   ,@(or (cdr (assoc 'typed_parameter nodes))
                                                    (cdr (assoc 'identifier nodes))))
                            (type             . ,(cdr (assoc 'type nodes)))
                            (parameters       . ,(cdr (assoc 'identifier nodes)))
                            (return_statement . ,(cdr (assoc 'return_statement nodes)))
                            (return_type_hint . ,(cdr (assoc 'return_type_hint nodes)))
                            (has_return       . ,(or (cdr (assoc 'return_statement nodes))
                                                     (cdr (assoc 'anonymous_return nodes))
                                                     (cdr (assoc 'return_type_hint nodes))))
                            (has_return       . ,(cdr (assoc 'arbitrary_args nodes)))
                            (kwargs           . ,(cdr (assoc 'kwargs nodes))))
                          results))
    results))

(defconst neogen-configuration-python
  (neogen-config-create
   :func '(([function_definition] [0]
            neogen-configuration-python-func))
   :class '(([class_definition] [0]
             (lambda (node)
               (let* ((tree '(:retrieve first
                             :node-type block
                             :subtree ((:retrieve first
                                                  :node-type function_definition
                                                  :subtree '((:retrieve first
                                                              :node-type block
                                                              :subtree ((:retrieve all
                                                                         :node-type expression-statement
                                                                         :subtree ((:retrieve first
                                                                                    :node-type assignment
                                                                                    :subtree ((:retrieve first
                                                                                               :node-type identifier
                                                                                               :as attributes
                                                                                               :extract t))))))))))))
                      (nodes (neogen-tree-query node tree))
                      (attrs (seq-filter (lambda (n)
                                           (not (string-prefix-p "_" (tsc-node-text n))))
                                         (cdr (assoc 'attributes nodes)))))
                 (setf (cdr (rassoc 'attributes nodes)) attrs)
                 nodes))))
   :file '(([module] [0]
            (lambda (node)
              '())))))


;;; PHP
(defconst neogen-configuration-php
  (neogen-config-create
   :type'(([property_declaration const_declaration foreach_statement] [0]
           (lambda (node)
             (let ((tree '(:node-type property_element
                                      :retrieve all
                                      :extract t
                                      :as type)))
               (neogen-tree-query node tree)))))
   :func '(([function_definition method_declaration] [0]
            (lambda (node)
              (let ((tree '((:node-type formal_parameters
                                        :retrieve first
                                        :subtree ((:node-type variable_name
                                                              :retrieve all
                                                              :extract t
                                                              :as parameters)))
                            (:node-type compound_statement
                                        :retrieve first
                                        :subtree ((:retrieve first
                                                             :node-type return_statement
                                                             :recursive t
                                                             :extract t
                                                             :as return_statement))))))
                (neogen-tree-query node tree)))))
   :class '(([class_declaration] [0]
             (lambda (node)
               '())))))


;;; Lua

(defun neogen-configuration-lua--extract-from-var (node)
  (let ((tree '((:retrieve first
                           :node-type assignment_statement
                           :subtree ((:retrieve first
                                                :node-type variable_list
                                                :subtree ((:retrieve all
                                                                     :node-type identifier
                                                                     :extract t)))))
                (:position 1
                           :extract t)))))
  (neogen-tree-query node tree))

(defconst neogen-configuration-lua
  (neogen-config-create
   :func '(([variable_declaration assignment_statement field] [0]
            (lambda (node)
              (neogen-configuration-lua--function-extractor node 'local)))
           ([function_declaration] [0]
            (lambda (node)
              (neogen-configuration-lua--function-extractor node 'function))))
   :class '(([local_variable_declaration variable_declaration] [0]
             (lambda (node)
               (let ((nodes (neogen-configuration-lua--extract-from-var node)))
                 `((class_name . (alist-get 'identifier nodes)))))))
   :type '(([local_variable_declaration variable_declaration] [0]
            (lambda (node)
              (let ((nodes (neogen-configuration-lua--extract-from-var node)))
                (cond
                 ((alist-get '_ nodes)
                  (push `(type . ,(alist-get '_ nodes)) nodes))
                 ((or (alist-get 'identifier nodes)
                      (alist-get 'field_expression nodes))
                  (push `(type . "any") nodes)))
                nodes))))
   :file '(([chunk] [0]
            (lambda (node)
              '())))))

;;; Templates

(defconst neogen-template-jsdoc
  '((nil "/* $1 */" ((type . (func class))
                     (no-results . t)))
    (nil "/* @type $1 */" ((type . (type))
                           (no-results . t)))

    (nil "/**" ((type . (file))
                (no-results . t)))
    (nil " * @module $1" ((type . (file))
                          (no-results . t)))
    (nil " */" ((type . (file))
                (no-results . t)))

    (nil "/**" ((type . (func class))))
    (class_name " * @classdesc $1" ((type . (class))
                                    (before-first-item . (" * " " * @class"))))
    (parameters " * @param {any} %s $1" ((type . (func))))
    ((type parameters) " * @param {%s} %s $1" ((type . (func))
                                               (required . tparam)))
    (return_statement " * @returns {$1} $1" ((type . (func))
                                             (limit . once)))
    (nil " */" ((type . (class func))))))

(defconst neogen-template-godoc
  '((nil " $1" ((type . (func type))
                (no-results . t)))))

(defconst neogen-template-google-bash
  '((nil "#######################################" ((no-results . t)
                                                    (type . (func))))
    (nil "# $1" ((no-results . t)
                 (type . (func))))
    (nil "#######################################" ((no-results . t)
                                                    (type . (func))))
    (nil "#!/bin/bash$1" ((no-results . t)
                          (type . (file))))
    (nil "#" ((no-results . t)
              (type . (file))))
    (nil "# $1" ((no-results . t)
                 (type . (file))))
    (nil "" ((no-results .  t)
             (type . (file))))
    (nil "#######################################")
    (nil "# $1")
    (variable_name "#   %s" ((before-first-item . ("# Globals:"))))
    (parameters "#   $1" ((before-first-item . ("# Arguments:"))))
    (nil "#######################################")))

(defconst neogen-template-doxygen
  '((nil "/**" ((no-results . t)
                (type . (func file class))))
    (nil " * @file" ((no-results . t)
                     (type . (file))))
    (nil " * @brief $1" ((no-results . t)
                         (type . (func file class))))
    (nil " */" ((no-results . t)
                (type . (func file class))))

    (nil "/**" ((type . (func file class))))
    (class_name " * @class %s" ((type . (class))))
    (type " * @typedef %s" ((type . (type))))
    (nil " * @brief $1" ((type . (func class type))))
    (nil " *" ((type . (func class type))))
    (tparam " * @tparam %s $1")
    (parameters " * @param %s $1")
    (return_statement " * @return $1")
    (nil " */" ((type . (func class type))))))

(defconst neogen-template-rdoc
  '((nil "##" ((type . (class func))))
    (nil "# $1" ((type . (class func))))
    (nil "" ((type . (class func))))
    (nil "# $1" ((type . (type))))

    (nil "##" ((no-results . t)
               (type . (class func))))
    (nil "# $1" ((no-results . t)
                 (type . (class func))))
    (nil "" ((no-results . t)
             (type . (class func))))
    (nil "# $1" ((no-results . t)
                 (type . (type))))))

(defconst neogen-template-rustdoc
  '((nil "! $1" ((no-results . t)
                 (type . (file))))
    (nil "" ((no-results . t)
             (type . (file))))

    (nil "/ $1" ((no-results . t)
                 (type . (func class))))
    (nil "/ $1" ((type . (func class))))))

(defconst neogen-template-rust-alternative
  '((nil "! $1" ((no-results . t)
                 (type . (file))))
    (nil "" ((no-results . t)
             (type . (file))))

    (nil "/ $1" ((no-results . t)
                 (type . (func class))))

    (nil "/ $1" ((type . (func class))))
    (nil "/" ((type . (func class))))
    (parameters "/ * `%s`: $1" ((type . (func))))
    (parameters "/ * `%s`: $1" ((type . (class))))))

(defconst neogen-template-google-docstrings
  '((nil "\"\"\" $1 \"\"\"" ((no-results . t)
                             (type . (class func))))
    (nil "\"\"\"$1"         ((no-results . t)
                             (type . (file))))
    (nil ""                 ((no-results . t)
                             (type . (file))))
    (nil "$1"               ((no-results . t)
                             (type . (file))))
    (nil "\"\"\""           ((no-results . t)
                             (type . (file))))
    (nil ""                 ((no-results . t)
                             (type . (file))))

    (nil "# $1" ((no-results . t)
                 (type . (type))))

    (nil                   "\"\"\"$1")
    (has_parameters        ""      ((limit . once)))
    (has_parameters        "Args:" ((limit . once)))
    (parameters            "    %s ($1): $1")
    ((parameters type) "    %s (%s): $1" ((required . typed_parameters) (type . (func))))
    (arbitrary_args        "    %s: $1"      ((type . (func))))
    (kwargs                "    %s: $1"      ((type . (func))))
    (attributes            "    %s: $1"      ((before-first-item . ("" "Attributes: "))))
    (has_return            ""                ((type . (func))))
    (has_return            "Returns:"        ((type . (func))))
    (has_return            "    $1"          ((type . (func))))
    (nil "\"\"\"")))

(defconst neogen-template-numpydoc
  '((nil "\"\"\" $1 \"\"\"" ((no-results . t)
                             (type . (class func))))
    (nil "\"\"\"$1" ((no-results . t)
                     (type . (file))))
    (nil "" ((no-results . t)
             (type . (file))))
    (nil "$1" ((no-results . t)
               (type . (file))))
    (nil "\"\"\"" ((no-results . t)
                   (type . (file))))
    (nil "" ((no-results . t)
             (type . (file))))

    (nil "# $1" ((no-results . t)
                 (type . (type))))

    (nil "\"\"\"$1")
    (has_parameters "" ((type . (func))
                        (limit . once)))
    (has_parameters "Parameters" ((type . (func))
                                  (limit . once)))
    (has_parameters "----------" ((type . (func))
                                  (limit . once)))
    (parameters "%s : $1" ((after-each . "    $1")
                           (type . (func))))
    ((parameters type) "%s : %s" ((after-each . "    $1")
                                  (required . typed_parameters)
                                  (type . (func))))
    (arbitrary_args "%s" ((after-each . "    $1")
                          (type . (func))))
    (kwargs "%s" ((after-each . "    $1")
                  (type . (func))))
    (attributes "%s : $1" ((before-first-item . ("" "Attributes" "----------"))))
    (has_return "" ((type . (func))
                    (limit . once)))
    (has_return "Returns" ((type . (func))
                           (limit . once)))
    (has_return "-------" ((type . (func))
                           (limit . once)))
    (return_type_hint "%s" ((after-each . "    $1")))
    (return_statement "$1" ((after-each . "    $1")
                            (limit . once)))
    (nil "\"\"\"")))

(defconst neogen-template-phpdoc
  '((nil "/** @var $1 */" ((no-result . t)
                           (type . (type))))

    (nil "/**" ((no-results . t)
                (type . (func class))))
    (nil " * $1" ((no-results . t)
                  (type . (func class))))
    (nil " */" ((no-results . t)
                (type . (func class))))

    (nil "/**" ((type . (type func))))
    (nil " * $1" ((type . (func))))
    (nil " *" ((type . (func))))
    (type " * @var $1" ((type . (type))))
    (parameters " * @param $1 %s $1" ((type . (func))))
    (return_statement " * @return $1" ((type . (func))))
    (nil " */" ((type . (type func))))))

(defvar neogen-mode-configuration-alist
  '((sh-mode neogen-configuration-sh neogen-template-google-bash)
    (c-mode neogen-configuration-c neogen-template-doxygen)
    (ruby-mode neogen-configuration-ruby neogen-template-rdoc)
    (python-mode neogen-configuration-python neogen-template-numpydoc)
    (php-mode neogen-configuration-php neogen-template-phpdoc)
    (typescript-mode neogen-configuration-typescript neogen-template-jsdoc)
    (tsx-mode neogen-configuration-typescript neogen-template-jsdoc)
    (rjsx-mode neogen-configuration-javascript neogen-template-jsdoc)
    (javascript-mode neogen-configuration-javascript neogen-template-jsdoc)))
