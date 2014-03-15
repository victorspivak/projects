<?php
class MyClassLoader
{
	public static function load($class)  {
	    $code = file_get_contents($class.'.php');
	    $code = str_replace('<?php', '', $code);
	    $code = str_replace(' final ', ' ', $code);
	    eval($code);
	}
}
spl_autoload_register('MyClassLoader::load');
