<?php
class A
{
	static function foo()
 	{
		static::get_name();
		self::get_name();
		A::get_name();
		B::get_name();

		static::get_again_name();
 	}

	static function get_name()
 	{
    	print "A\n";
 	}

	static function get_again_name()
 	{
		static::get_name();
		self::get_name();
		A::get_name();
		B::get_name();
 	}
}
 
class B extends A
{
	static function foo()
 	{
		static::get_name();
		self::get_name();
		A::get_name();
		B::get_name();

		static::get_again_name();
 	}

	static function get_name()
 	{
    	print "B\n";
 	}

	static function get_again_name()
 	{
		static::get_name();
		self::get_name();
		A::get_name();
		B::get_name();
 	}
}
 
$a = new A();
$b = new B();

$a->foo();
print("-------------------\n");
$b->foo();
?>
