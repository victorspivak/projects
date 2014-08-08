<?php

$arr = array('a' => 1, 'b' => 2, 'c' => 3, 'd' => 4, 'e' => 5);
$json = json_encode($arr);
print ("$json\n");

class User implements \JsonSerializable
{
    private $name;
    private $email;
    private $age;

    function __construct($name, $email, $age)
    {
        $this->age = $age;
        $this->email = $email;
        $this->name = $name;
    }

    function __toString()
    {
        return "$this->name  $this->email   $this->age";
    }

    public function JsonSerialize()
    {
        $vars = get_object_vars($this);

        return $vars;
    }
}

class File implements \JsonSerializable
{
    private $name;
    private $size;
    private $owner;
    private $author;

    function __construct($name, $size, $author, $owner)
    {
        $this->author = $author;
        $this->name = $name;
        $this->owner = $owner;
        $this->size = $size;
    }

    function __toString()
    {
        return "$this->name  $this->size  owner: $this->owner  author: $this->author";
    }

    public function JsonSerialize()
    {
        $vars = get_object_vars($this);

        return $vars;
    }
}

$owner = new User("John", "john@gmail.com", 22);
$author = new User("Peter", "peter@gmail.com", 33);
$file = new File('My best file', 12345, $author, $owner);

print ("$file\n");
$json = json_encode($file);
print ("$json\n");

print("---------------------------------------------------\n");
class User1
{
    public $name;
    public $email;
    public $age;

    function __construct($name, $email, $age)
    {
        $this->age = $age;
        $this->email = $email;
        $this->name = $name;
    }

    function __toString()
    {
        return "$this->name  $this->email   $this->age";
    }
}

class File1
{
    public $name;
    public $size;
    public $owner;
    public $author;

    function __construct($name, $size, $author, $owner)
    {
        $this->author = $author;
        $this->name = $name;
        $this->owner = $owner;
        $this->size = $size;
    }

    function __toString()
    {
        return "$this->name  $this->size  owner: $this->owner  author: $this->author";
    }
}

$owner = new User1("John", "john@gmail.com", 22);
$author = new User1("Peter", "peter@gmail.com", 33);
$file = new File1('My best file', 12345, $author, $owner);

print ("$file\n");
$json = json_encode($file);
print ("$json\n");



