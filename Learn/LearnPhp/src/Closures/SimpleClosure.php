<?php
	test();

	function test()
	{
		$result = 0;
		tracer(function () use(&$result){   //allows to pass a variable to the closure by address
			print("Hello World\n");
			$result = 200;
		}, $result);
	    print("Result: " . $result);
	}

	function tracer($callable)
	{
		print("Starting...\n");
		$callable();
		print("Done.\n");
	}
?>
