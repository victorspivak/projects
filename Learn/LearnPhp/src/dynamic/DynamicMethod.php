<?php

class Welcome
{
	public static function __callStatic($method, $arguments)
	{
		printf("%s %s\n", $method, $arguments[0]);
	}
}

Welcome::hi("Vic");

class MethodTest
{
    public function __call($name, $arguments)
    {
        // Note: value of $name is case sensitive.
        echo "Calling object method '$name' "
             . implode(', ', $arguments). "\n";
    }

    /**  As of PHP 5.3.0  */
    public static function __callStatic($name, $arguments)
    {
        // Note: value of $name is case sensitive.
        echo "Calling static method '$name' "
             . implode(', ', $arguments). "\n";
    }
}

$obj = new MethodTest;
$obj->runTest('in object context');

MethodTest::runTest('in static context');  // As of PHP 5.3.0



function title($title, $name)
{
    return sprintf("%s. %s\r\n", $title, $name);
}

$function = new ReflectionFunction('title');

echo $function->invoke('Dr', 'Phil');

echo call_user_func('title', 'Dr', 'Phil');
echo call_user_func_array('title', array('Dr', 'Phil'));

class Greeting
{
	function greet($first, $last)
	{
		return "Hello " . $first . " " . $last . "\n";
	}
}

$greeting = new Greeting();

echo call_user_func(array($greeting, 'greet'), 'Victor', 'Spivak');
echo call_user_func_array(array($greeting, 'greet'), array('Victor', 'Spivak'));

?>
