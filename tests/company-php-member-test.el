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
