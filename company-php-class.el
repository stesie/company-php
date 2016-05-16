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
(require 'company-php-member)		; for regexp definitions
(require 'cl-lib)


(setq company-php-class--use-stmt-regex
      (concat "\\<use\\>\s+"
	      "\\(" "\\(?:" company-php-member--classname-regex "\\\\" "\\)*"
	      "\\(" company-php-member--classname-regex
	      "\\)" "\\)"
	      ";"))


(setq company-php-class--candidates-mapping nil)


;;;###autoload
(defun company-php-class-backend (command &optional arg &rest ignored)
  "Company backend for PHP class name completion."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-php-class-backend))
    (prefix      (company-php-class--prefix))
    (meta        (company-php-class--meta arg))
    (annotation  (company-php-class--annotation arg))
    (candidates  (company-php-class--candidates arg))))

;;;###autoload
(defun company-php-class--capf ()
  "completion-at-point function for PHP class name completion."
  (let ((prefix (company-php-class--prefix)))
    (when prefix
      (unless company-php-class--candidates-mapping
	(company-php-class--fetch-candidates))
      (list (match-beginning 1)
	    (point)
	    (company-php-class--candidates "")
	    :annotation-function #'company-php-class--annotation
	    :company-docsig #'company-php-class--meta))))

(defun company-php-class--prefix ()
  "Get completion prefix"
  (and
   (eq major-mode 'php-mode)
   (let ((limit (save-excursion (c-beginning-of-statement 1) (point))))
     (looking-back "\\(?:use\\|new\\)\s+\\(\\(?:\\\\?[a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*\\)*\\)" limit))
   (match-string-no-properties 1)))

(defun company-php-class--meta (candidate)
  "Get completion candidate meta data"
  (get-text-property 0 'short-desc candidate))

(defun company-php-class--annotation (candidate)
  "Get completion candidate annotation string"
  ;(unless (equal candidate (get-text-property 0 'class-name candidate))
  (format " (\\%s)" (get-text-property 0 'class-name candidate)))

(defun company-php-class--candidates (prefix)
  "Get completion candidates"
  (unless company-php-class--candidates-mapping
    (company-php-class--fetch-candidates))

  (copy-sequence ; hack so company won't clobber our list
   (cl-remove-if-not
    (lambda (c) (string-prefix-p prefix c))
    (mapcar (lambda (candidate)
	      (let* ((class-info   (cdr (assoc "class" candidate)))
		     (descriptions (cdr (assoc "descriptions" class-info)))
		     (short-name   (progn (string-match "[a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*$" (car candidate))
					  (match-string 0 (car candidate)))))
		(propertize short-name
			    'short-desc (cdr (assoc "short" descriptions))
			    'class-name (car candidate))))
	    company-php-class--candidates-mapping))))

(defun company-php-class--fetch-candidates ()
  "Read JSON data from index file"
  (let ((index (company-php-read-index "classes")))
    (when index
      (setq company-php-class--candidates-mapping (cdr (assoc "mapping" index))))))

(defun company-php-class--get-uses ()
  "Extract use mapping from current buffer"
  (let (uses)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward company-php-class--use-stmt-regex nil t)
	(push (cons (match-string 1) (match-string 2)) uses))
      uses)))

(provide 'company-php-class)
