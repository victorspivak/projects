<?php

function increment(&$var)
{
    $var++;
}

$a = 1;
call_user_func('increment', $a);
echo $a."\n";

call_user_func_array('increment', array(&$a)); // You can use this instead before PHP 5.3
echo $a."\n";

class Person {
    private $_name;
    static $_person_count = 0;

    function __construct($name) {
        $this->_name = $name;
        Person::$_person_count += 1;
    }

    function name () {
        return $this->_name;
    }

    static function personCount() {
        return Person::$_person_count;
    }
}

$obj = new Person("Vic");

$n = call_user_func(array($obj, "name"));
echo "$n\n";

$n = call_user_func(array("Person", "personCount"));
echo "$n\n";

?>
