<?php
class A
{
    static $class_name = 'A';
 
    function function_that_calls_static_function_in_B()
    {
        return B::static_function();
    }
 
    function function_that_calls_dynamic_function_in_B()
    {
        return B::dynamic_function();
    }
}
 
class B
{
    static $class_name = 'B';
 
    static function static_function()
    {
        return static::$class_name;
    }
 
    function dynamic_function()
    {
        return static::$class_name;
    }
}
 
$a = new A();
print $a->function_that_calls_static_function_in_B();// prints 'B'
print $a->function_that_calls_dynamic_function_in_B();// prints 'A' since the scope is not 'B' but '$a'!!!!
?>
