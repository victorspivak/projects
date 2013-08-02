<?php

class MyBase
{
	public function validate()
	{
		$class = get_called_class();
		$class_methods = get_class_methods($class);

		foreach($class_methods as $method)
		{
			$reflectionMethod = new ReflectionMethod($class, $method);
			$doc = $reflectionMethod->getDocComment();
			print ($doc . "\n");
			//if(preg_match_all('/[\*\s](?=@)/', $doc, $matches, PREG_OFFSET_CAPTURE)) {
			if(preg_match_all("/((?:\/\*(?:[^*]|(?:\*+[^*\/]))*\*+\/)|(?:\/\/.*))/", $doc, $matches)) {
				print("Hello\n");
			}

			foreach($matches as $match)
			{
				print_r ($match);

			}
		}

	}
}

class MyChild extends MyBase
{
	/**
	 * It is a test
	 * @validation(Min("100") Max("200")) @Range(100, 300)*/
	public function setFoo1($val)
	{
		$this->foo1 = $val;
	}

	/** @Min("100") @Max("300") */
	public function setFoo2($val)
	{
		$this->foo2 = $val;
	}
}

$var = new MyChild();
$var->setFoo1(50);
$var->setFoo2(50);
$var->validate();

?>
