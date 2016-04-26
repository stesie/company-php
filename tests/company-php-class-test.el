(require 'company-php-class)

(ert-deftest company-php-class--prefix-test ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (setq major-mode 'php-mode)
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
       (setq major-mode 'php-mode)
       (insert (car config))
       (should (string= (company-php-class--prefix) (cdr config)))))
   '(("new \\Foo" . "\\Foo")
     ("new \\Foo\\Bar" . "\\Foo\\Bar")
     ("new \\Foo\\Bar\\Baz" . "\\Foo\\Bar\\Baz"))))

(ert-deftest company-php-class--meta ()
  (let ((company-php-class--candidates-mapping
	 '(("PHPUnit_Framework_TestCase" .
	    (("class" .
	      (("descriptions" .
		(("short" . "A TestCase defines the fixture to run multiple tests.")
		 ("long" . "To define a TestCase ..."))))))))))
    (should (string=
	     (company-php-class--meta "PHPUnit_Framework_TestCase")
	     "A TestCase defines the fixture to run multiple tests."))))

(ert-deftest company-php-class--candidates ()
  (let ((company-php-class--candidates-list
	 '("Assert\\Assertion"
	   "Assert\\AssertionChain"
	   "Assert\\AssertionFailedException"
	   "PHPUnit_Framework_TestCase")))
    (mapcar
     (lambda (config)
       (should (equal
		(company-php-class--candidates (car config))
		(cdr config))))
     '(("Assert" . ("Assert\\Assertion"
		    "Assert\\AssertionChain"
		    "Assert\\AssertionFailedException"))
       ("PHPU" . ("PHPUnit_Framework_TestCase"))
       ("Foo" . nil)))))
