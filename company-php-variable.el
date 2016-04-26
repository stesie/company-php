(defconst company-php-variable-regex
  "\\$[a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*")

;;;### autoload
(defun company-php-variable-backend (command &optional arg &rest ignored)
  "Company backend for PHP variable name completion."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-php-variable-backend))
    (prefix      (company-php-variable--prefix))
    (candidates  (company-php-variable--candidates arg))))

(defun company-php-variable--prefix ()
  "Get completion prefix"
  (and
   (eq major-mode 'php-mode)
   (looking-back company-php-variable-regex)
   (match-string 0)))

(defun company-php-variable--candidates (prefix)
  "Get completion candidates"
  (cl-remove-if-not
   (lambda (c)
     (and (not (string= prefix c))
	  (string-prefix-p prefix c)))
   (company-php-variable--extract-variables)))

(defun company-php-variable--extract-variables ()
  "Get list of variables used within function at point"
  (save-excursion
    (php-end-of-defun)
    (let ((defun-end (point))
	  (variables '("$this")))
      (php-beginning-of-defun)
      (while (re-search-forward company-php-variable-regex defun-end 'noerror)
	(unless (member (match-string 0) variables)
	  (push (match-string 0) variables)))
      variables)))

(provide 'company-php-variable)
