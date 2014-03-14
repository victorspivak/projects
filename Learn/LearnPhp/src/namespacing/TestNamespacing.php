<?php
function __autoload($className)  {
    $file = str_replace('\\', DIRECTORY_SEPARATOR, $className) . '.php';
    if (!file_exists($file)) {
        return false;
    }
    else {
        require $file;
        return true;
    }
}

use \hr\Person as Person;

$vicAddress = new \hr\address\Address('1654 De Anza blvd', 'San Mateo', 'CA', '94403');
$person1 = new \hr\Person('Victor', $vicAddress);
$person2 = new Person('Victor', $vicAddress);
$address = new \address\Address('1654 De Anza blvd', 'San Mateo', 'CA', '94403');

var_dump($person1->getName());
print($person1->getAddress());