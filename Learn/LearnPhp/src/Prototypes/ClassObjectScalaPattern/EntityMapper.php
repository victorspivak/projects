<?php

namespace Prototypes\ClassObjectScalaPattern;

class EntityMapper
{
	private static $instance;
	protected $allKeys = [];
	protected $keys = [];

	public static function instance()
	{
		if (!self::$instance){
			self::$instance = new EntityMapper();
		}
		return self::$instance;
	}

	public function &getAllKeysRef()
	{
		return $this->allKeys;
	}

	public function &getKeysRef()
	{
		return $this->keys;
	}

	public function dumpData(){
		print_r ($this->allKeys);
		print_r ($this->keys);
	}
}
