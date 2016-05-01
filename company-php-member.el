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

(defun company-php-member--get-full-class-name ()
  (save-excursion
    (re-search-backward "\\<class\s+\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)")
    (let ((class-name (match-string 1)))
      (if (re-search-backward "\\<namespace\s+\\(\\(?:[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\\\\\)*[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)" nil t)
	  (concat (match-string 1) "\\" class-name)
	class-name))))

(defun company-php-member--get-candidates (prefix)
  (company-php-member--fetch-candidates)
  (mapcar
   (lambda (value)
     (car value))
   company-php-member--candidates))

(defun company-php-member--fetch-candidates ()
  (setq company-php-member--candidates
	(cdr (assoc "values"
		    (company-php--run-helper
		     "methods"
		     (company-php-member--get-full-class-name))))))

;;;### autoload
(defun company-php-member-backend (command &optional arg &rest ignored)
  "Company backend for PHP class members completion."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-php-member-backend))
    (prefix      (company-php-member--prefix))
    (candidates  (company-php-member--get-candidates arg))))

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
				  "\\(" company-php-member--classname-regex "\\)"))
	  (setq result (cons (match-string 1) (point))))

	(c-beginning-of-statement 1))
      result)))


(provide 'company-php-member)
