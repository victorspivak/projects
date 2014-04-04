<?php

namespace Prototypes\ClassObjectScalaPattern;

require_once 'EntityMapper.php';

class Entity
{
	protected static $allKeys;
	protected static $keys;

	public static function setData()
	{
		self::$allKeys = &EntityMapper::instance()->getAllKeysRef();
		self::$keys = &EntityMapper::instance()->getKeysRef();

		self::$allKeys['k1'] = 'v1';
		self::$allKeys['k2'] = 'v2';

		$keys = [
			'k10' => 'v10',
			'k11' => 'v11'
		];

		self::$keys = $keys;

		print_r (self::$allKeys);
		print_r (self::$keys);
	}
} 
