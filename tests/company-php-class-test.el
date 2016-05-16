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

(require 'company-php-class)

(ert-deftest company-php-class--prefix-test ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (let ((php-mode-hook nil))
	 (php-mode))
       (insert (car config))
       (should (string= (company-php-class--prefix) (cdr config)))))
   '(("new" . nil)
     ("new " . "")
     ("new Foo" . "Foo")
     ("new Foo_Bar" . "Foo_Bar")
     ("new   FooBar" . "FooBar")
     ("use" . nil)
     ("use " . "")
     ("use Foo" . "Foo"))))

(ert-deftest company-php-class--prefix-test-namespaces ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (let ((php-mode-hook nil))
	 (php-mode))
       (insert (car config))
       (should (string= (company-php-class--prefix) (cdr config)))))
   '(("new \\Foo" . "\\Foo")
     ("new \\Foo\\Bar" . "\\Foo\\Bar")
     ("new \\Foo\\Bar\\Baz" . "\\Foo\\Bar\\Baz"))))

(ert-deftest company-php-class--meta ()
  (let ((candidate (propertize "PHPUnit_Framework_TestCase"
			       'short-desc "A TestCase defines the fixture to run multiple tests.")))
    (should (string=
	     (company-php-class--meta candidate)
	     "A TestCase defines the fixture to run multiple tests."))))

(ert-deftest company-php-class--candidates ()
  (let ((company-php-class--candidates-mapping
	 '(("Assert\\Assertion")
	   ("Assert\\AssertionChain")
	   ("Assert\\AssertionFailedException")
	   ("PHPUnit_Framework_TestCase"))))
    (mapcar
     (lambda (config)
       (should (equal
		(cl-letf (((symbol-function #'company-php-class--do-fqcn-completion)
			   (lambda () nil)))
		  (company-php-class--candidates (car config)))
		(cdr config))))
     '(("Assert" . ("Assertion"
		    "AssertionChain"
		    "AssertionFailedException"))
       ("PHPU" . ("PHPUnit_Framework_TestCase"))
       ("Foo" . nil)))))

(ert-deftest company-php-class--get-uses ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "use QafooLabs\\Refactoring\\Domain\\Model\\File;
use QafooLabs\\Refactoring\\Domain\\Model\\LineRange;")
    (should (equal
	     (company-php-class--get-uses)
	     '(("QafooLabs\\Refactoring\\Domain\\Model\\LineRange" . "LineRange")
	       ("QafooLabs\\Refactoring\\Domain\\Model\\File" . "File"))))))

(ert-deftest company-php-class--get-uses-with-rename ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "use QafooLabs\\Refactoring\\Domain\\Model\\File as SomeFile;
use QafooLabs\\Refactoring\\Domain\\Model\\LineRange;")
    (should (equal
	     (company-php-class--get-uses)
	     '(("QafooLabs\\Refactoring\\Domain\\Model\\LineRange" . "LineRange")
	       ("QafooLabs\\Refactoring\\Domain\\Model\\File" . "SomeFile"))))))

(ert-deftest company-php-class--candidates-handles-class-aliases ()
  (let ((company-php-class--candidates-mapping
	 '(("Assert\\Assertion")
	   ("Assert\\AssertionChain")
	   ("Assert\\AssertionFailedException")
	   ("PHPUnit_Framework_TestCase"))))
    (cl-letf (((symbol-function #'company-php-class--get-uses)
	       (lambda ()
		 '(("Assert\\Assertion" . "Assertion")
		   ("Assert\\AssertionChain" . "AliasChain")
		   ("Assert\\AssertionFailedException" . "AssertFailException"))))
	      ((symbol-function #'company-php-class--do-fqcn-completion)
			   (lambda () nil)))
      (should (equal
	       (company-php-class--candidates "Assert")
	       '("Assertion"
		 "AssertFailException"))))))
