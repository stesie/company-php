(require 'php-mode)
(require 'company-php)
(require 'company-php-member)

(ert-deftest company-php-member--classpath-regex ()
  (mapcar
   (lambda (config)
     (with-temp-buffer
       (let ((php-mode-hook nil))
	 (php-mode))
       (insert (car config))
       (beginning-of-buffer)
       (should (equal (looking-at company-php-member--classpath-regex) (cdr config)))))
   '(("Foo" . t)
     ("Foo\Bar" . t)
     ("Foo\Bar\Baz" . t)
     ("\Foo\Bar" . t))))

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
   '(("class Foo { }" . "\\Foo")
     ("class Bar extends Foo { }" . "\\Bar")
     ("class Baz implements \\Countable { }" . "\\Baz")
     ("namespace Foo\\Bar;
class Baz { }" . "\\Foo\\Bar\\Baz"))))

(ert-deftest company-php-member--qualify-class-name-simple-namespace ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "namespace Foo;")
    (should (string= (company-php-member--qualify-class-name "Bar")
		     "\\Foo\\Bar"))))

(ert-deftest company-php-member--qualify-class-name-namespace-with-slash ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "namespace Foo\\Bar;")
    (should (string= (company-php-member--qualify-class-name "Baz")
		     "\\Foo\\Bar\\Baz"))))

(ert-deftest company-php-member--qualify-class-name-simple-fqcn ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "namespace Foo;")
    (should (string= (company-php-member--qualify-class-name "\\Bar")
		     "\\Bar"))))

(ert-deftest company-php-member--qualify-class-name-use-with-alias ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "namespace Foo;
use My\\Full\\Classname as Another;")
    (should (string= (company-php-member--qualify-class-name "Another")
		     "\\My\\Full\\Classname"))))

(ert-deftest company-php-member--qualify-class-name-use ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "namespace Foo;
use My\\Full\\Classname;")
    (should (string= (company-php-member--qualify-class-name "Classname")
		     "\\My\\Full\\Classname"))))


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
    (cl-letf (((symbol-function 'company-php--run-helper)
	       (lambda (command class-name)
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
			       ("isStatic" . nil)))))))))
      (should (equal (company-php-member--get-candidates "")
		     '("foo"))))))

(ert-deftest company-php-member--guess-type-from-typehint-test ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo(Bar $bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-type-from-typehint "$bar")
	     '("Bar" . 45)))))

(ert-deftest company-php-member--guess-type-from-typehint-mismatch ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo(Bar $bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-type-from-typehint "$foo")
	     nil))))

(ert-deftest company-php-member--guess-type-from-typehint-considers-range ()
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
	     (company-php-member--guess-type-from-typehint "$foo")
	     nil))))


(ert-deftest company-php-member--guess-type-from-typehint-fqcn ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo(\\Assert\\Assertion $assertion)
    {
        ")
    (should (equal
	     (company-php-member--guess-type-from-typehint "$assertion")
	     '("\\Assert\\Assertion" . 65)))))

(ert-deftest company-php-member--guess-type-from-docblock ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    /**
     * @param Bar $bar
     */
    public function foo($bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-type-from-docblock "$bar")
	     '("Bar" . 43)))))

(ert-deftest company-php-member--guess-type-from-docblock-fqcn ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    /**
     * @param \\Some\\Nice\\Bar $bar
     */
    public function foo($bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-type-from-docblock "$bar")
	     '("\\Some\\Nice\\Bar" . 54)))))

(ert-deftest company-php-member--guess-type-from-docblock-missing ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    /**
     * @param Foo $foo
     */
    public function foo($foo, $bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-type-from-docblock "$bar")
	     nil))))

(ert-deftest company-php-member--guess-type-from-docblock-no-docblock ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    /**
     * @param Bar $bar
     */
    public function bar($bar)
    {
        echo 23;
    }

    public function foo($bar)
    {
        ")
    (should (equal
	     (company-php-member--guess-type-from-docblock "$bar")
	     nil))))

(ert-deftest company-php-member--guess-type-from-variable-docblock ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        /**
         * @var Bar $bar
         */
        $bar = $this->somethingReturningSomething();
        ")
    (should (equal
	     (company-php-member--guess-type-from-variable-docblock "$bar")
	     '("Bar" . 81)))))

(ert-deftest company-php-member--guess-type-from-variable-docblock-fqcn ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        /**
         * @var \\Some\\Nice\\Bar $bar
         */
        $bar = $this->somethingReturningSomething();
        ")
    (should (equal
	     (company-php-member--guess-type-from-variable-docblock "$bar")
	     '("\\Some\\Nice\\Bar" . 92)))))

(ert-deftest company-php-member--guess-type-from-variable-docblock-reverse-notation ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        /**
         * @var $bar Bar
         */
        $bar = $this->somethingReturningSomething();
        ")
    (should (equal
	     (company-php-member--guess-type-from-variable-docblock "$bar")
	     '("Bar" . 81)))))

(ert-deftest company-php-member--guess-type-from-variable-docblock-reverse-notation-fqcn ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        /**
         * @var $bar \\Some\\Nice\\Bar
         */
        $bar = $this->somethingReturningSomething();
        ")
    (should (equal
	     (company-php-member--guess-type-from-variable-docblock "$bar")
	     '("\\Some\\Nice\\Bar" . 92)))))

(ert-deftest company-php-member--guess-type-from-variable-docblock-inline ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        /** @var Bar $bar */
        $bar = $this->somethingReturningSomething();
        ")
    (should (equal
	     (company-php-member--guess-type-from-variable-docblock "$bar")
	     '("Bar" . 70)))))

(ert-deftest company-php-member--guess-type-from-variable-docblock-inline-fqcn ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        /** @var \\Some\\Nice\\Bar $bar */
        $bar = $this->somethingReturningSomething();
        ")
    (should (equal
	     (company-php-member--guess-type-from-variable-docblock "$bar")
	     '("\\Some\\Nice\\Bar" . 81)))))

(ert-deftest company-php-member--guess-type-from-variable-docblock-none ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        $bar = $this->somethingReturningSomething();
        ")
    (should (equal
	     (company-php-member--guess-type-from-variable-docblock "$bar")
	     nil))))

(ert-deftest company-php-member--guess-type-from-assignment-new ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        $bar = new Foo();
        ")
    (should (equal
	     (company-php-member--guess-type-from-assignment "$bar")
	     '("Foo" . 53)))))

(ert-deftest company-php-member--guess-type-from-assignment-new-2 ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        $bar = new Foo();
        $bar = new Bar();
        ")
    (should (equal
	     (company-php-member--guess-type-from-assignment "$bar")
	     '("Bar" . 79)))))

(ert-deftest company-php-member--guess-type-from-assignment-new-3 ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        $bar = new Foo();
        $blarfoo = new Bar();
        ")
    (should (equal
	     (company-php-member--guess-type-from-assignment "$bar")
	     '("Foo" . 53)))))

(ert-deftest company-php-member--guess-type-from-assignment-new-fqcn ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        $bar = new \\Some\\Other\\Foo();
        $blarfoo = new Bar();
        ")
    (should (equal
	     (company-php-member--guess-type-from-assignment "$bar")
	     '("\\Some\\Other\\Foo" . 53)))))

(ert-deftest company-php-member--guess-type-from-assignment-new-none ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        $bar = new Foo();
        $bar = new Bar();
        ")
    (should (equal
	     (company-php-member--guess-type-from-assignment "$foo")
	     nil))))

(ert-deftest company-php-member--guess-type-from-assignment-new-in-other-method ()
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "class Bar
{
    public function foo()
    {
        $foo = new Foo();
        $foo = new Bar();
    }

    public function foo()
    {
        $bar = 23;
        ")
    (should (equal
	     (company-php-member--guess-type-from-assignment "$foo")
	     nil))))

(ert-deftest company-php-member--get-class-name-from-stack-toplevel ()
  "If the completion is on top-level, i.e. immediate member of an object
   referenced by a variable, the guess-type-from functions need to be
   called (and the one with max point should win)."
  (with-temp-buffer
    (let ((php-mode-hook nil))
      (php-mode))
    (insert "namespace Foo;
class Bar
{
    public function foo(Foo $foo)
    {
        $foo = new Baz(); // <- type Baz overrides Foo here
")
    (should (equal
	     (company-php-member--get-class-name-from-stack '("$foo"))
	     "\\Foo\\Baz"))))
