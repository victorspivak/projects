<?php

class Foo {
    public function greet() {
        echo "Hi\n";
        self::Hello();
        Foo::Hello();
    }

    public static function Hello() {
        echo "Hello\n";
    }
}

$obj = new Foo();
$obj->greet();

Foo::Hello();

?>

