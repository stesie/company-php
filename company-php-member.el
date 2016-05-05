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
	 (current-class-parents (cdr (assoc "parents" (company-php--run-helper "methods" current-class-name)))))
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

(defun company-php-member--fetch-candidates ()
  (let ((class-name (substring (company-php-member--get-class-name-from-stack
				(company-php-member--get-stack)) 1)))
    (setq company-php-member--candidates
	  (cdr (assoc "values"
		      (company-php--run-helper "methods" class-name))))))

(defun company-php-member--get-class-name-from-stack (stack)
  (company-php-member--get-variable-type (car stack)))

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


;;;### autoload
(defun company-php-member-backend (command &optional arg &rest ignored)
  "Company backend for PHP class members completion."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-php-member-backend))
    (prefix      (company-php-member--prefix))
    (meta        (company-php-member--get-meta arg))
    (annotation  (company-php-member--get-annotation arg))
    (candidates  (company-php-member--get-candidates arg))))

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
