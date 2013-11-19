<?php
	public function monitor_timing(Callable $callable)
	{
		$start_time = microtime(true);
		$callable();
		return microtime(true) - $start_time;
	}
