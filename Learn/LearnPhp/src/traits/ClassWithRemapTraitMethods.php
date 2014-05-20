<?php

trait MyTrait
{
    public function foobar()
    {
        printf("Foo is called.\n");
        parent::foobar();
    }

	function __call($name, $arguments)
	{
	}
}

class Foo
{
    public function foobar()
    {
        printf("foobar:: It is me.\n");
    }
    public function bar()
    {
        printf("bar:: It is me.\n");
    }
}

class TraitAndClass extends Foo
{
    use MyTrait{
		foobar as MyTrait_foobar;
		bar as MyTrait_bar;
	}
//    use MyTrait;

	public function foobar()
	{
		printf("foobar:: TraitAndClass.\n");
		$this->MyTrait_foobar();
	}
	public function bar()
	{
		printf("bar:: TraitAndClass.\n");
	}
}

$o = new TraitAndClass();
$o->foobar();

if ($o instanceof Foo)
    printf("It is Foo.\n");

if ($o instanceof MyTrait)
    printf("It is DecoratorTrait.\n");

if (class_uses($o)['MyTrait'])
    printf("It is DecoratorTrait using class_uses.\n");

