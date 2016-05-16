;; Copyright (C) 2016 Stefan Siegl

;; Author: Stefan Siegl <stesie@brokenpipe.de>

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(require 'company-php)
(require 'company-php-variable)

(setq company-php-member--prefix-regex
      "\\(?:\\$[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\s*->\s*\\(?:[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\s*\\(?:(.*?)\\)?\s*->\s*\\)*\\([a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*\\)")

(setq company-php-member--classname-regex
      "\\<[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\>")

(setq company-php-member--classpath-regex
      (concat "\\\\?"
	      "\\(?:" company-php-member--classname-regex "\\\\" "\\)*"
	      company-php-member--classname-regex))


(defun company-php-member--prefix ()
  "Get completion prefix"
  (and
   (eq major-mode 'php-mode)
   (looking-back company-php-member--prefix-regex)
   (match-string 1)))

(defun company-php-member--get-stack ()
  (save-excursion
    (let ((end (point)) result)
      (re-search-backward company-php-member--prefix-regex)

      (re-search-forward company-php-variable-regex end)
      (push (match-string 0) result)

      (while (re-search-forward "\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\\(?:\s*(.*?)\\)?\s*->" end 'noerror)
	(push (match-string 1) result))

      (reverse result))))

(defun company-php-member--qualify-class-name (class-name)
  (if (string-prefix-p "\\" class-name)
      class-name			; already FQCN
    (save-excursion
      (cond
       ;; match use-statement with aliasing
       ((re-search-backward (concat "\\<use\s+"
				    "\\(" company-php-member--classpath-regex "\\)"
				    "\s+as\s+" (regexp-quote class-name) "\s*;") nil t)
	(concat "\\" (match-string 1)))

       ;; use-statement without aliasing
       ((re-search-backward (concat "\\<use\s+"
				    "\\("
				    "\\(?:" company-php-member--classname-regex "\\\\" "\\)*"
				    (regexp-quote class-name) "\\)" "\s*;") nil t)
	(concat "\\" (match-string 1)))

       ;; prefix classname with namespace
       ((re-search-backward "\\<namespace\s+\\(\\(?:[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\\\\\)*[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)" nil t)
	(concat "\\" (match-string 1) "\\" class-name))

       ;; no namespace, assume qualified classname
       (t (concat "\\" class-name))))))


(defun company-php-member--get-full-class-name ()
  (save-excursion
    (re-search-backward "\\<class\s+\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)")
    (company-php-member--qualify-class-name (match-string 1))))

(defun company-php-member--get-candidates (prefix)
  (company-php-member--fetch-candidates)
  (let* ((current-class-name (substring (company-php-member--get-full-class-name) 1))
	 (current-class-parents (cdr (assoc "parents" (company-php-member--get-methods current-class-name)))))
    (mapcar
     (lambda (value)			; map name from members
       (car value))

     (cl-remove-if-not
      (lambda (elm)
	(let* ((info (if (= (length elm) 2) (cadr elm) elm))
	       (declaring-class (cdr (assoc "name" (assoc "declaringClass" info)))))

	  (and
	   ;; ignore static properties (but accept methods)
	   (or (not (cdr (assoc "isStatic" info)))
	       (cdr (assoc "isMethod" info)))

	   ;; ignore invisible members
	   (or (cdr (assoc "isPublic" info))
	       (and (cdr (assoc "isProtected" info))
		    (member declaring-class current-class-parents))
	       (and (or (cdr (assoc "isPrivate" info))
			(cdr (assoc "isProtected" info)))
		    (string= declaring-class current-class-name)))

	   ;; hide __construct
	   (not (string= "__construct" (car elm)))

	   ;; match only members having names starting with "prefix"
	   (string-prefix-p prefix (car elm)))))
      company-php-member--candidates))))


(setq company-php-member--methods-cache
      (make-hash-table :test 'equal))

(defun company-php-member--get-methods (class-name)
  (or (gethash class-name company-php-member--methods-cache)
      (puthash class-name (company-php--run-helper "methods" class-name)
	       company-php-member--methods-cache)))

(setq company-php-member--autocomplete-cache
      (make-hash-table :test 'equal))

(defun company-php-member--get-autocomplete (class-name member-name)
  (let ((cache-key (concat class-name "|" member-name)))
    (or (gethash cache-key company-php-member--autocomplete-cache)
	(puthash cache-key (company-php--run-helper "autocomplete" class-name member-name)
		 company-php-member--autocomplete-cache))))




(defun company-php-member--fetch-candidates ()
  (let ((class-name (substring (company-php-member--get-class-name-from-stack
				(company-php-member--get-stack)) 1)))
    (setq company-php-member--candidates
	  (cdr (assoc "values" (company-php-member--get-methods class-name))))))

(defun company-php-member--get-class-name-from-stack (stack)
  (let ((type (company-php-member--get-variable-type (car stack))))
    (while (cdr stack)
      (setq stack (cdr stack))
      (setq type (company-php-member--get-member-type type (car stack))))
    type))


(defun company-php-member--get-variable-type (variable-name)
  (if (string= variable-name "$this")
      (company-php-member--get-full-class-name)
    (let (result)
      (dolist (fn company-php-member--guess-type-function-list)
	(let ((match (funcall fn variable-name)))
	  (when (and match
		     (or (not result)
			 (> (cdr match) (cdr result))))
	    (setq result match))))

      (when result
	(company-php-member--qualify-class-name (car result))))))

(defun company-php-member--get-member-type (class-name member-name)
  (let* ((meta-info (company-php-member--get-autocomplete class-name member-name)))
    (concat "\\" (cdr (assoc "class" meta-info)))))


;;;###autoload
(defun company-php-member-backend (command &optional arg &rest ignored)
  "Company backend for PHP class members completion."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-php-member-backend))
    (prefix      (company-php-member--prefix))
    (meta        (company-php-member--get-meta arg))
    (annotation  (company-php-member--get-annotation arg))
    (candidates  (company-php-member--get-candidates arg))))

;;;###autoload
(defun company-php-member--capf ()
  "completion-at-point function for PHP class members completion."
  (let ((prefix (company-php-member--prefix)))
    (when prefix
      (list (match-beginning 1)
	    (point)
	    (company-php-member--get-candidates "")
	    :company-docsig #'company-php-member--get-meta
	    :exit-function #'company-php-member--exit-function
	    :annotation-function #'company-php-member--get-annotation))))

(defun company-php-member--get-meta (member)
  (let* ((member-info   (assoc member company-php-member--candidates))
	 (args          (assoc "args" member-info))
	 (descriptions  (assoc "descriptions" args)))
    (cdr (assoc "short" descriptions))))

(defun company-php-member--get-annotation (member)
  (let* ((member-info   (assoc member company-php-member--candidates))
	 (args          (assoc "args" member-info))
	 (parameters    (cdr (assoc "parameters" args)))
	 (optionals     (cdr (assoc "optionals" args)))
	 (return-type   (cdr (assoc "type" (assoc "return" args)))))

    (if (cdr (assoc "isMethod" member-info))
	(concat "("
		(mapconcat 'identity parameters ", ")
		(when optionals
		  (concat (when parameters " ")
			  "["
			  (when parameters ", ")
			  (mapconcat 'identity optionals ", ")
			  "]"))
		") -> " return-type)

      (concat " @var " return-type))))

(defun company-php-member--exit-function (member status)
  "Completion on-exit function, adding parentheses after member name, if the
completed member is a method.  If the member takes no arguments, then a
ellipsis is inserted without further interaction.  Otherwise yasnippet expansion
is triggered for all parameters."
  (let* ((member-info   (assoc member company-php-member--candidates))
	 (args          (assoc "args" member-info))
	 (parameters    (cdr (assoc "parameters" args))))

    (when (cdr (assoc "isMethod" member-info))
      (if (not parameters)
	  (insert "()")	   ; auto-close method calls without arguments
	(if (bound-and-true-p yas-minor-mode)
	    (let ((index 0))
	      (yas-expand-snippet
	       (concat "("
		       (mapconcat 'identity
				  (mapcar (lambda (elm)
					    (incf index)
					    (format "${%d:%s}" index elm))
					  parameters) ", ")
		       ")$0")))
	  (insert "("))))))


(defun company-php-member--guess-type-from-typehint (var-name)
  "Try to guess type of variable from typehint"
  (save-excursion
    (php-beginning-of-defun)
    (let ((scope (save-excursion
		   (re-search-forward ")") ; goto end of function parameters
		   (point))))
      (if (re-search-forward
	   (concat "\\(" company-php-member--classpath-regex "\\)\s+"
		   (regexp-quote var-name))
	   scope t)
	  (cons (match-string 1) (point))
	nil))))

(defun company-php-member--guess-type-from-docblock (var-name)
  "Try to guess type of variable from function docblock"
  (save-excursion
    (php-beginning-of-defun)
    (when (looking-back "\\*\\/\s*\n")
      (let ((scope (point)))
	(re-search-backward "\\/\\*\\*")
	(if (re-search-forward
	     (concat "@param\s+\\(" company-php-member--classpath-regex "\\)\s+"
		     (regexp-quote var-name))
	     scope t)
	    (cons (match-string 1) (point)))))))

(defun company-php-member--guess-type-from-variable-docblock (var-name)
  "Try to guess type of variable from function internal variable docblock."
  (save-excursion
    (let ((scope (point)))
      (php-beginning-of-defun)
      (or
       ;; try to match @var ClassName $inst
       (and (re-search-forward (concat "\\/\\*\\*[\s\n]*\\(?:\\*\s+\\)?"
				       "@var\s+\\(" company-php-member--classpath-regex "\\)\s+"
				       (regexp-quote var-name))
			       scope t)
	    (cons (match-string 1) (point)))
       ;; try to match @var $inst ClassName
       (and (re-search-forward (concat "\\/\\*\\*[\s\n]*\\(?:\\*\s+\\)?"
				       "@var\s+" (regexp-quote var-name)
				       "\s+\\(" company-php-member--classpath-regex "\\)")
			       scope t)
	    (cons (match-string 1) (point)))))))

(defun company-php-member--guess-type-from-assignment (var-name)
  "Try to guess type of variable from the last variable assignment."
  (let ((scope (save-excursion
		 (php-beginning-of-defun)
		 (point)))
	result)
    (save-excursion
      (while (and
	      (not result)
	      (> (point) scope))
	(when (looking-at (concat (regexp-quote var-name)
				  "\s*=\s*new\s+"
				  "\\(" company-php-member--classpath-regex "\\)"))
	  (setq result (cons (match-string 1) (point))))

	(c-beginning-of-statement 1))
      result)))


(setq company-php-member--guess-type-function-list
      '(company-php-member--guess-type-from-typehint
	company-php-member--guess-type-from-docblock
	company-php-member--guess-type-from-assignment))

(provide 'company-php-member)
