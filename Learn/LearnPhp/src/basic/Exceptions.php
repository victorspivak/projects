<?php

function f1()
{
	try{
		f2();
	}
	catch (Exception $e)
	{
		$tr = $e->getTrace();
		foreach($tr as $elem)
		{
			$file = $elem['file'];
			$line = $elem['line'];
			$function = $elem['function'];
			print("$file  $line  $function\n");
		}
	}
}

function f2()
{
	f3();
}

function f3()
{
	f4();
}

function f4()
{
	throw new Exception("It is a fake exception");
}

f1();

?>
