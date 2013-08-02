<?php

function foo($p)
{
    print("I am $p\n");
}

$res = foo('Victor');

if ($res  === 'no_access')
{
	print('I am in place 1');
}

if (is_array($res))
{
	print('I am in place 2');
}

if (isset($res))
{
	print('I am in place 3');
}

if ($res === null)
{
	print("I am in place 4 \n");
}

if ($res)
{
	print("I am in place 5 \n");
}

print("Variable |$res| \n");

?>
