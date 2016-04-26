(ert-deftest company-php-member--prefix-test ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (let ((php-mode-hook nil))
	 (php-mode))
       (insert (car config))
       (should (string= (company-php-member--prefix) (cdr config)))))
   '(("$foo->" . "")
     ("$foo->set" . "set")
     ("$foo_bar->get" . "get")
     ("$foo->bar->baz" . "baz")
     ("$foo->bar()->baz" . "baz")
     ("$foo->bar ()->baz" . "baz")
     ("$foo->bar () ->baz" . "baz")
     ("$foo->bar(23)->baz" . "baz")
     ("$foo->bar(23)->bar(42)->bar(5)->baz" . "baz")
     ("$foo->bar(23, $this)->baz" . "baz")
     ("$foo -> bar(23, $this) -> baz" . "baz")
     ("bla" . nil))))

(ert-deftest company-php-member--get-stack-test ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (let ((php-mode-hook nil))
	 (php-mode))
       (insert (car config))
       (should (equal (company-php-member--get-stack) (cdr config)))))
   '(("$foo->" . ("$foo"))
     ("$bar->" . ("$bar"))
     ("$bar->set" . ("$bar"))
     ("$foo_bar->get" . ("$foo_bar"))
     ("$foo->bar->baz" . ("$foo" "bar"))
     ("$foo->bar()->baz" . ("$foo" "bar"))
     ("$foo->bar (23, 42)->baz" . ("$foo" "bar")))))

(ert-deftest company-php-member--get-full-class-name ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (let ((php-mode-hook nil))
	 (php-mode))
       (insert (car config))
       (should (equal (company-php-member--get-full-class-name)
		      (cdr config)))))
   '(("class Foo { }" . "Foo")
     ("class Bar extends Foo { }" . "Bar")
     ("class Baz implements \\Countable { }" . "Baz")
     ("namespace Foo\\Bar;
class Baz { }" . "Foo\\Bar\\Baz"))))

(ert-deftest company-php-member--candidates-test-this ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "namespace Foo;
class Bar
{
    public function foo()
    {
        $this->")
    (flet ((company-php--run-helper
	    (command class-name)
	    (should (string= command "methods"))
	    (should (string= class-name "Foo\\Bar"))
	    '(("wasFound" . t)
	      ("class" . "\\Foo\\Bar")
	      ("shortName" . "Bar")
	      ("isTrait" . nil)
	      ("isClass" . t)
	      ("isAbstract" . nil)
	      ("isInterface" . nil)
	      ("parents" . nil)
	      ("values" .
	       (("foo" . (("isMethod" . t)
			  ("isProperty" . nil)
			  ("isPublic" . t)
			  ("isProtected" . nil)
			  ("isPrivate" . nil)
			  ("isStatic" . nil))))))))
      (should (equal (company-php-member--get-candidates "")
		     '("foo"))))))

(ert-deftest company-php-member--guess-class-from-typehint-test ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo(Bar $bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-class-from-typehint "$bar")
	     '("Bar" . 37)))))

(ert-deftest company-php-member--guess-class-from-typehint-mismatch ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo(Bar $bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-class-from-typehint "$foo")
	     nil))))

(ert-deftest company-php-member--guess-class-from-typehint-considers-range ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function bar(Foo $foo)
    {
        /* nothing here */
    }

    public function foo(Bar $bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-class-from-typehint "$foo")
	     nil))))
