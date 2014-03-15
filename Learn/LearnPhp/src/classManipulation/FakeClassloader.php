<?php

class FakeClassloader
{
	public static function load($class)  {
		$code = "class $class {public function run() {echo '$class';}" .
  			'public function __call($name,$args) {
          $args=implode(",",$args);
          echo "$name ($args)<br>";
      		}
		  }';

  		eval($code);
	}
}

spl_autoload_register('FakeClassloader::load');
