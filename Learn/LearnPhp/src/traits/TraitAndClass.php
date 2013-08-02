<?php

trait DecoratorTrait
{
    public function foobar()
    {
        printf("Foo is called.\n");
        parent::foobar();
    }
}

class Foo
{
    public function foobar()
    {
        printf("It is me.\n");
    }
}

class TraitAndClass extends Foo
{
    use DecoratorTrait;
}

$o = new TraitAndClass();
$o->foobar();

if ($o instanceof Foo)
    printf("It is Foo.\n");

if ($o instanceof DecoratorTrait)
    printf("It is DecoratorTrait.\n");

if (class_uses($o)['DecoratorTrait'])
    printf("It is DecoratorTrait using class_uses.\n");
