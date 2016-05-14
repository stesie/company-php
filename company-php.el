;;; company-php.el --- Company (auto completion) backend for PHP

;; Copyright (C) 2016 Stefan Siegl

;; Author: Stefan Siegl <stesie@brokenpipe.de>
;; URL: https://github.com/stesie/company-php
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

(defconst company-php-version-number "0.0.1"
  "company-php version number.")

(defconst company-php-modified "2016-05-05"
  "company-php build date.")

;;; License

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

(require 'company)
(require 'cl-lib)

(defgroup company-php nil
  "company-mode backend for PHP programming language"
  :prefix "company-php-"
  :group 'php)

(defcustom company-php--php-program
  "/usr/bin/php"
  "Path to PHP program to run composer and reflection helper with."
  :type 'string
  :group 'company-php)

(defcustom company-php--composer-program
  "/usr/bin/composer"
  "company-php depends on composer to generate classmap files.  Specify the
path to your composer bin (e.g. bin/composer, composer.phar, /usr/bin/composer)."
  :type 'string
  :group 'company-php)

(defcustom company-php--autoload-files
  '("vendor/autoload.php" "autoload.php")
  "List of relative paths to the files of autoload.php from composer.
Usually this is just \"vendor/autoload.php\" for composer projects, but you may
specify multiple paths if you have different paths for some projects."
  :type 'string
  :group 'company-php)

(defcustom company-php--classmap-files
  '("vendor/composer/autoload_classmap.php" "autoload/ezp_kernel.php")
  "List of relative paths to files that contain a classmap (array with \"className\" => \"fileName\").
By default on composer it's vendor/composer/autoload_classmap.php"
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
