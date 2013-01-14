<?php
    class Base
    {
        private $_name;
        function __construct()
        {
            printf("Base Constructor\n");
        }
        function __destruct()
        {
            printf("Base Destructor\n");
        }

        function name($name = null) {
            if (isset($name))
                $this->_name = $name;

            return $this->_name;
        }
    }

    class Child extends Base
    {
        function __construct()
        {
            parent::__construct();
            parent::name("Child");  //in the constructor you must use parent
            printf("Child Constructor\n");
        }
        function __destruct()
        {
            printf("Child Destructor\n");
            parent::__destruct();
        }
    }

    $baseObj = new Base();
    $baseObj = null;

    $childObj = new Child();
    print("name: " . $childObj->name()."\n");
    $childObj = null;
?>