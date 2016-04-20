(require 'company)
(require 'cl-lib)

(setq company-php-class--candidates-list nil)
(setq company-php-class--candidates-mapping nil)

;;;### autoload
(defun company-php-class-backend (command &optional arg &rest ignored)
  "Company backend for PHP class name completion."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-php-class-backend))
    (prefix      (company-php-class--prefix))
    (meta        (company-php-class--meta arg))
    (candidates
     (copy-sequence ; hack so company won't clobber our list
      (progn
	(unless company-php-class--candidates-list
	  (company-php-class--fetch-candidates))
	(cl-remove-if-not
	 (lambda (c) (string-prefix-p arg c))
	 company-php-class--candidates-list))))))

(defun company-php-class--prefix ()
  "Get completion prefix"
  (and
   (eq major-mode 'php-mode)
   (looking-back "\\(?:use\\|new\\)\s+\\(\\(?:\\\\?[a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*\\)*\\)")
   (match-string 1)))

(defun company-php-class--meta (candidate)
  "Get completion candidate meta data"
  (let* ((candidate-info (cdr (assoc candidate company-php-class--candidates-mapping)))
	 (class-info     (cdr (assoc "class" candidate-info)))
	 (descriptions   (cdr (assoc "descriptions" class-info))))
    (cdr (assoc "short" descriptions))))

(defun company-php-class--fetch-candidates ()
  "Read JSON data from index file"
  (let* ((json-array-type 'list)
	 (json-key-type 'string)
	 (index (json-read-file "/home/stesie/Projekte/atom-autocomplete-php/indexes/32779d1a7c218f27164747b5ddf61728/index.classes.json")))
    (setq company-php-class--candidates-list (cdr (assoc "autocomplete" index)))
    (setq company-php-class--candidates-mapping (cdr (assoc "mapping" index)))))

(provide 'company-php-class)
