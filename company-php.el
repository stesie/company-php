(require 'company)
(require 'cl-lib)

(defgroup company-php nil
  "company-mode backend for PHP programming language"
  :prefix "company-php-"
  :group 'php)

;;;###autoload
(defcustom company-php--php-program
  "/usr/bin/php"
  "Default PHP program"
  :type 'string
  :group 'company-php)

(defcustom company-php--composer-program
  "/usr/bin/composer"
  "Default PHP Composer program"
  :type 'string
  :group 'company-php)

(defcustom company-php--autoload-files
  '("vendor/autoload.php" "autoload.php")
  "Default Autoload files"
  :type 'string
  :group 'company-php)

(defcustom company-php--classmap-files
  '("vendor/composer/autoload_classmap.php" "autoload/ezp_kernel.php")
  "Default Classmap files"
  :type 'string
  :group 'company-php)



(defvar company-php--external-helper-dir
  (concat
   (file-name-directory
    (or load-file-name
	(buffer-file-name)))
   "atom-autocomplete-php/php")
  "Path to the external PHP helper")

(defvar company-php-project-path nil
  "Project root directory")

(defun company-php-read-index (name)
  "Read an index by its name (indexes/index.[name].json)"
  (unless company-php-project-path
    (error "company-php-project-path variable not set"))
  (let* ((json-array-type 'list)
	 (json-key-type 'string)
	 (index-file-name
	  (concat
	   company-php--external-helper-dir "/../indexes/"
	   (secure-hash 'md5 company-php-project-path)
	   "/index." name ".json")))
    (json-read-file index-file-name)))

(defun company-php--run-helper (method &rest args)
  "Run external PHP helper"
  (with-temp-file (concat company-php--external-helper-dir "/tmp.php")
    (insert "<?php "
	    "$config = array("
	    "'composer' => '" company-php--composer-program "',"
	    "'php' => '" company-php--php-program "',"
	    "'autoload' => " (company-php--insert-array company-php--autoload-files) ","
	    "'classmap' => " (company-php--insert-array company-php--classmap-files) ","
	    ");"))
  (let* ((shell-result (shell-command-to-string
			(combine-and-quote-strings
			 (append
			  (list
			   company-php-program
			   (concat company-php--external-helper-dir "/parser.php")
			   company-php-project-path
			   (concat "--" method))
			  args))))
	 (json-array-type 'list)
	 (json-false nil)
	 (json-key-type 'string))
    (json-read-from-string shell-result)))

(defun company-php--insert-array (array)
  "Write PHP Array with strings from array"
  (apply
   #'concat
   (append '("array(")
	   (mapcar (lambda (elm) (concat "'" elm "',")) array)
	   '(")"))))


(provide 'company-php)
