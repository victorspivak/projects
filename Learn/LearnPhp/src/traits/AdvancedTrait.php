<?php

trait AutoIdTrait
{
    private static $id_generator = 0;
    private $id;

    public function getId()
    {
        if (!$this->id) $this->id = $this->generate_id();
        return $this->id;
    }

    private function generate_id()
    {
        return '' . (++self::$id_generator);
    }
}

class Foo
{
    use AutoIdTrait;
}

$o = new Foo();
printf ("%s\n", $o->getId());
printf ("%s\n", $o->getId());
printf ("%s\n", $o->getId());
$o = new Foo();
printf ("%s\n", $o->getId());

