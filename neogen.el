;;; neogen.el --- Annotation generator. -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((yasnippet "0.14.0"))
;; Homepage: https://github.com/zkry
;; Keywords: tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'tsc)
(require 'tree-sitter)

(cl-defstruct (neogen-config
               (:constructor neogen-config-create)
               (:copier nil))
  func file class type)

(defun neogen--supported-config-types (config)
  "Return a list of supported types from CONFIG."
  (let ((types '()))
    (when (neogen-config-func config)
      (push "func" types))
    (when (neogen-config-file config)
      (push "file" types))
    (when (neogen-config-class config)
      (push "class" types))
    (when (neogen-config-type config)
      (push "type" types))
    types))

;; this is for reference
(defconst neogen-configuration-template-items '(tparam
                                                parameters
                                                return_statement ;; Return
                                                return_type_hint
                                                return_anonym
                                                class_name
                                                throw_statement ;; Throw
                                                varargs
                                                type
                                                attributes ;; ClassAttribute
                                                has_parameters
                                                has_return
                                                arbitrary_args
                                                kwargs))

(defun neogen-tree-query* (node query)
  ""
  (let ((node-type (plist-get query :node-type))
        (retrieve (plist-get query :retrieve))
        (subtree (plist-get query :subtree))
        (recursive (plist-get query :recursive))
        (as (plist-get query :as))
        (extract (plist-get query :extract))
        (top-level (plist-get query :top-level))
        (position (plist-get query :position)))
    (if (vectorp node-type)
        (let ((acc '()))
          (seq-do (lambda (nt)
                    (let ((res (neogen-tree-query* node (plist-put query :node-type nt))))
                      (setq acc (append acc res))))
                  node-type)
          acc)
      (let* ((tsc-query (tsc-make-query tree-sitter-language
                                        (vector (list node-type) '@result)))
             (res-items (alist-get 'result (neogen--group-extractions (tsc-query-captures tsc-query node #'ts--buffer-substring-no-properties))))
             (extractions '()))
        (if (and position extract)
            (list (cons '_ (tsc-get-nth-child node position)))
          (when (and res-items (eql retrieve 'first))
              (setq res-items (list (car res-items))))
          (dolist (res-item res-items extractions)
            (when (or (not top-level)
                      (tsc-node-eq node (tsc-get-parent res-item)))
              (cond
               ((and extract as)
                (setq extractions (cons (cons as res-item) extractions)))
               (extract
                (setq extractions (cons (cons node-type res-item) extractions)))
               (subtree
                (let ((ress '()))
                  (unless (listp (car subtree))
                    (error "Subtree must be list of queries"))
                  (dolist (tree subtree)
                    (let ((res (neogen-tree-query* res-item tree)))
                      (setq ress (append ress res))))
                  (setq extractions (append extractions ress))))))))
        extractions))))

(defun neogen-tree-query (node query)
  (if (listp (car query))
      (let ((ress '()))
        (dolist (q query)
          (let ((res (neogen-tree-query* node q)))
            (setq ress (append ress res))))
        (neogen--group-extractions ress))
    (let ((res (neogen-tree-query* node query)))
      (neogen--group-extractions res))))

(defun neogen-config-get (config type)
  "Dynamically lookup TYPE in CONFIG."
  (cond
   ((eql type 'func) (neogen-config-func config))
   ((eql type 'file) (neogen-config-file config))
   ((eql type 'class) (neogen-config-class config))
   ((eql type 'type) (neogen-config-type config))
   (t (error "Unknown type %s" type))))

(defun neogen-template-filter (template type)
  "Return relevant data of TEMPLATE for TYPE."
  (seq-filter
   (lambda (stmt)
     (seq-let (_ _ metadata) stmt
       (let ((line-type (cdr (assoc 'type metadata))))
         (or (not line-type)
             (memq type line-type)))))
   template))

(defun neogen-locate (types idx)
  "Locate annotation node in the current tree for TYPES and IDX."
  (let* ((p (point))
         (at-node (tsc-get-descendant-for-position-range
                   (tsc-root-node tree-sitter-tree) p p)))
    (catch 'result
      (while t
        (when (not at-node)
          (throw 'result nil))
        (when (seq-some (lambda (type) (equal (tsc-node-type at-node) type))
                        types)
          (throw 'result at-node))
        (setq at-node (tsc-get-parent at-node))))))

(defun neogen-config-locate-any (language-config)
  "Given a LANGUAGE-CONFIG, find any annotation location an run it's extract function."
  (catch 'result
   (let ((types '(neogen-config-func neogen-config-class neogen-config-file neogen-config-type)))
     (dolist (type-fn types)
       (let ((type-configs (funcall type-fn language-config)))
         (dolist (config-line type-configs)
           (seq-let (tree-types idx extract-fn) config-line
             (let ((loc-node (neogen-locate tree-types idx)))
               (when loc-node
                 (throw 'result (list loc-node extract-fn)))))))))))

(defun neogen-config-locate (type-config type)
  "Given a TYPE-CONFIG find annotation location for TYPE an run it's extract function."
  (catch 'result
    (dolist (config-line type-config)
      (seq-let (tree-types idx extract-fn) config-line
        (let ((loc-node (neogen-locate tree-types idx)))
          (when loc-node
            (throw 'result (list loc-node extract-fn))))))))

(defun neogen--group-extractions (extractions)
  ""
  (seq-reduce (lambda (acc elt)
                (let* ((key (car elt))
                       (node (cdr elt))
                       (prev (cdr (assoc key acc))))
                  (when (not (listp node))
                    (setq node (list node)))
                  (cons (cons key (append prev node)) (assoc-delete-all key acc))))
              extractions
              '()))

(defun neogen-config-extract (node extractor)
  "Run EXTRACTOR function for NODE."
  (neogen--group-extractions (funcall extractor node)))

(defun neogen-generate-template (type template extractions)
  ""
  (let ((result '())
        (template (neogen-template-filter template type)))
    (dolist (template-item template)
      (seq-let (key line-str metadata) template-item
        (let-alist metadata
          (when .after-each
            (setq line-str (concat line-str "\n" .after-each)))
          (when (and (or (and .no-results (not extractions))
                         (and (not .no-results) extractions))
                     (or (not .required) (assoc .required extractions)))
            (when (and .before-first-item
                       (or (not key) (cdr (assoc key extractions))))
              (dolist (extra-line .before-first-item)
                (push extra-line result)))
            (when .required
              (dolist (req-item (seq-reverse (cdr (assoc .required extractions))))
                (let ((format-params (seq-map (lambda (k)
                                                (let ((node (cadr (assoc k req-item))))
                                                  (if node
                                                      (tsc-node-text (cadr (assoc k req-item)))
                                                    "$1")))
                                              key)))
                  (push (apply #'format (append (list line-str) format-params)) result))))
            (cond
             (key
              (let ((params (cdr (assoc key extractions))))
                (when params
                  (when (eql .limit 'once)
                    (setq params (list (car params))))
                  (dolist (param (seq-reverse params))
                    ;; TODO: Do no-param casese always exist as a nil value?
                    (cond
                     ((booleanp param)
                      (push line-str result))
                     ((listp param)
                      (dolist (p param)
                        (push (format line-str (tsc-node-text p)) result)))
                     ((stringp param)
                      (push (format line-str (tsc-node-text param)) result))
                     (t
                      (push (format line-str (tsc-node-text param)) result)))))))
             (t (push line-str result)))))))
    (nreverse result)))

(defun neogen-template-to-yas-snippet (template)
  "Convert TEMPLATE lines to a yas snippet."
  (let ((joined-template (string-join template "\n"))
        (i 1))
    (with-temp-buffer
      (insert joined-template)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "$1")
          (delete-char 2)
          (insert (format "$%d" i))
          (cl-incf i))
        (forward-char))
      (insert "\n")
      (buffer-string))))



(defvar neogen-overlay nil
  "Overlay to store styling for newly inserted documentation text.")

(defun neogen--change-advice (orig &rest args)
  ""
  (if (seq-some (lambda (ov)
                  (eql (overlay-get ov 'type) 'neogen-template))
                (overlays-at (point)))
     nil
    (apply orig args)))

(defun neogen-insert-template (node yas-snippet)
  "Insert documentation lines given the NODE to be documented and the YAS-SNIPPET."
  (let ((start (tsc-node-start-position node)))
    (goto-char start)
    (yas-expand-snippet yas-snippet)))

(defvar neogen-mode-configuration-alist
  '(("Shell-script" neogen-configuration-sh neogen-template-google-bash)
    ("C/*l" neogen-configuration-c neogen-template-doxygen)
    ("Ruby" neogen-configuration-ruby neogen-template-rdoc)
    ("Python" neogen-configuration-python neogen-template-numpydoc)
    ("PHP//l" neogen-configuration-php neogen-template-phpdoc)
    ("typescript" neogen-configuration-typescript neogen-template-jsdoc)))

(defun neogen-fetch-configuration ()
  "Return the lsp configuration associated with the current mode."
  (symbol-value (nth 1 (assoc mode-name neogen-mode-configuration-alist #'equal))))

(defun neogen-fetch-template ()
  "Return the documentation template associated with the current mode."
  (symbol-value (nth 2 (assoc mode-name neogen-mode-configuration-alist #'equal))))

(defun neogen (type)
  "Insert documentation comments of TYPE according to major-mode."
  (interactive
   (list
    (let ((config (neogen-fetch-configuration)))
      (unless config
        (error "No neogen configuration found for mode %s" mode-name))
      (intern (completing-read "Select type:" (neogen--supported-config-types config) nil t)))))
  (unless tree-sitter-mode
    (error "Mode tree-sitter-mode must be enabled"))
  (let ((config (neogen-fetch-configuration))
        (template (neogen-fetch-template)))
    
    (unless template
      (error "No neogen template found for mode %s" mode-name))
    (let ((type-config (neogen-config-get config type)))
      (unless type-config
        (error "Mode cannot generate documentation for type %s" type))
      (let ((loc-data (neogen-config-locate type-config type)))
        (unless loc-data
          (error "Unable to find code for documentation of type %s" type))
        (let* ((extract-data (apply #'neogen-config-extract loc-data))
               (template-lines (neogen-generate-template type template extract-data))
               (yas-snippet (neogen-template-to-yas-snippet template-lines)))
          (neogen-insert-template (car loc-data) yas-snippet))))))

(defun neogen-func ()
  "Insert function documentation according to major-mode.  Same as (neogen 'func)."
  (interactive)
  (neogen 'func))

(defun neogen-class ()
  "Insert class documentation according to major-mode.  Same as (neogen 'class)."
  (interactive)
  (neogen 'class))

(defun neogen-type ()
  "Insert type documentation according to major-mode.  Same as (neogen 'type)."
  (interactive)
  (neogen 'type))

(defun neogen-file ()
  "Insert file documentation according to major-mode.  Same as (neogen 'file)."
  (interactive)
  (neogen 'file))

(provide 'neogen)

;;; neogen.el ends here
