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
	      "\\(?:\s+as\s+\\(" company-php-member--classname-regex "\\)\\)?"
	      "\s*;"))

(setq company-php-class--prefix-regex
      "\\(use\\|new\\)\s+\\(\\(?:\\\\?[a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*\\)*\\)")

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
      (list (match-beginning 2)
	    (point)
	    (company-php-class--candidates "")
	    :annotation-function #'company-php-class--annotation
	    :company-docsig #'company-php-class--meta))))

(defun company-php-class--prefix ()
  "Get completion prefix"
  (and
   (eq major-mode 'php-mode)
   ;; work around using looking-back which took ages to fail
   ;; with this regexp: just re-search-backward and make sure the matching
   ;; string runs at least to the point
   (let ((limit (save-excursion (c-beginning-of-statement 1) (point))))
     (save-excursion
       (re-search-backward company-php-class--prefix-regex limit t)))
   (>= (match-end 2) (point))
   (match-string-no-properties 2)))

(defun company-php-class--do-fqcn-completion ()
  "Test prefix whether to do FQCN completion or not.
Returns 'slashed if FQCN completion should be performed and the completion
candidates should have a backslash at the beginning.
Returns non-nil if FQCN completion is required otherwise."
  (let ((prefix (company-php-class--prefix)))
    (if (string-prefix-p "\\" prefix)
	'slashed
      (or (string-match-p "\\\\" prefix)
	  (string= "use" (match-string-no-properties 1))))))

(defun company-php-class--meta (candidate)
  "Get completion candidate meta data"
  (get-text-property 0 'short-desc candidate))

(defun company-php-class--annotation (candidate)
  "Get completion candidate annotation string"
  (unless (get-text-property 0 'fqcn-completion candidate)
    (format " (\\%s)" (get-text-property 0 'class-name candidate))))

(defun company-php-class--candidates (prefix)
  "Get completion candidates"
  (unless company-php-class--candidates-mapping
    (company-php-class--fetch-candidates))

  (let ((uses            (company-php-class--get-uses))
	(fqcn-completion (company-php-class--do-fqcn-completion)))
    (cl-remove-if-not
     (lambda (c) (string-prefix-p prefix c))
     (mapcar (lambda (candidate)
	       (let* ((class-info   (cdr (assoc "class" candidate)))
		      (descriptions (cdr (assoc "descriptions" class-info)))
		      (short-name   (progn (string-match "[a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*$" (car candidate))
					   (match-string 0 (car candidate))))
		      (alias        (assoc (car candidate) uses)))
		 (propertize (cond ((equal fqcn-completion 'slashed) (concat "\\" (car candidate)))
				   (fqcn-completion (car candidate))
				   (alias           (cdr alias))
				   (t               short-name))
			     'fqcn-completion fqcn-completion
			     'short-desc      (cdr (assoc "short" descriptions))
			     'class-name      (car candidate))))
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
	(push (cons (match-string-no-properties 1)
		    (or (match-string-no-properties 3)
			(match-string-no-properties 2))) uses))
      uses)))

(provide 'company-php-class)
