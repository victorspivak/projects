<?php

class myclass {
    function myclass()
    {
        return(true);
    }

    function myfunc1()
    {
        return(true);
    }

    public function myfunc2()
    {
        return(true);
    }

    private function privateMethod1()
    {
        return(true);
    }

    protected function protectedMethod1()
    {
        return(true);
    }

    static function staticMethod1()
    {
        return(true);
    }
}

$class_methods = get_class_methods('myclass');

foreach ($class_methods as $method_name) {
    echo "$method_name\n";
}

echo "-----------------------------------------------------\n";

$class_methods = get_class_methods(new myclass());

foreach ($class_methods as $method_name) {
    echo "$method_name\n";
}

echo get_class(new myclass())  . "\n";
?>

