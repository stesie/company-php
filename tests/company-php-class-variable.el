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
  (flet ((company-php-variable--extract-variables ()
						  '("$the_a"
						    "$the_arg"
						    "$this")))
    (should (equal
	     (company-php-variable--candidates "$the_a")
	     '("$the_arg")))))
