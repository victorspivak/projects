<?php
trait MyTrait
{
	function foo($count = 0)
	{
		print ("MyTrait::foo\n");
	}
}

class Test1
{
	use MyTrait;

	function foo()
	{
		print ("Test1::foo\n");
	}
}

class BaseTest
{
	use MyTrait;
}

class Test2 extends BaseTest
{
	public static $data = [];

	function foo()
	{
		print ("Test2::foo\n");
	}
	function foobar()
	{
		print ("Test2::foo\n");
	}
}

$o1 = new Test1();
$o1->foo();
$o2 = new Test2();
$o2->foo();

$methods = get_class_methods ('Test2');
print_r($methods);

$set1 = isset(Test2::$data);
$set2 = isset(Test2::$data1);

print (" set 1: $set1\n");
print (" set 2: $set2\n");


