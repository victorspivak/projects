<?php

trait DecoratorTrait
{
    public function foobar()
    {
		printf("DecoratorTrait: Foo is called.\n");
    }
}

class Foo
{
//    public function foobar()
//    {
//		printf("Foo: Foo is called.\n");
//    }
}

class DecoratedClass extends Foo
{
	use DecoratorTrait;
//	public function foobar()
//	{
//		printf("DecoratedClass: Foo is called.\n");
//		parent::foobar();
//	}
}

class TraitAndClass extends DecoratedClass
{
	public function foobar()
	{
		printf("TraitAndClass: Foo is called.\n");
		parent::foobar();
	}
}

$o = new TraitAndClass();
$o->foobar();

echo "------------------------------------------------\n";
if ($o instanceof Foo)
    printf("It is Foo.\n");

if ($o instanceof DecoratorTrait)
    printf("It is DecoratorTrait.\n");

if (class_uses($o)['DecoratorTrait'])
    printf("It is DecoratorTrait using class_uses.\n");
