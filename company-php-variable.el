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

(defconst company-php-variable-regex
  "\\$[a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*")

;;;###autoload
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
