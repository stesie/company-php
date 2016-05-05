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

(require 'company)
(require 'company-php)
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
    (candidates  (company-php-class--candidates arg))))

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

(defun company-php-class--candidates (prefix)
  "Get completion candidates"
  (unless company-php-class--candidates-list
    (company-php-class--fetch-candidates))

  (copy-sequence ; hack so company won't clobber our list
   (cl-remove-if-not
    (lambda (c) (string-prefix-p prefix c))
    company-php-class--candidates-list)))

(defun company-php-class--fetch-candidates ()
  "Read JSON data from index file"
  (let ((index (company-php-read-index "classes")))
    (when index
      (setq company-php-class--candidates-list (cdr (assoc "autocomplete" index)))
      (setq company-php-class--candidates-mapping (cdr (assoc "mapping" index))))))

(provide 'company-php-class)
