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

(require 'php-mode)
(require 'company-php-variable)

(ert-deftest company-php-variable--prefix-test ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (let ((php-mode-hook nil))
	 (php-mode))
       (insert (car config))
       (should (string= (company-php-variable--prefix) (cdr config)))))
   '(("new" . nil)
     ("$foo" . "$foo")
     ("$foo_bar" . "$foo_bar")
     ("$foo23" . "$foo23"))))

(ert-deftest company-php-variable--extract-variables-test ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Foo
{
    public function theFunctionBefore($this_should_not_show_up)
    {
        $this_neither = 23;
    }

    public function doSomething($the_arg)
    {
        $the_a = 23;
    }

    public function theFunctionAfter($the_arg_that_should_not_show_up_too)
    {
        $the_b = 23;
    }
}")
    (goto-line 10)			; within doSomething
    (should (equal
	     (company-php-variable--extract-variables)
	     '("$the_a"
	       "$the_arg"
	       "$this")))))

(ert-deftest company-php-variable--candidates ()
  (cl-letf (((symbol-function 'company-php-variable--extract-variables)
	     (lambda ()
	       '("$the_a"
		 "$the_arg"
		 "$this"))))
    (should (equal
	     (company-php-variable--candidates "$the_a")
	     '("$the_arg")))))
