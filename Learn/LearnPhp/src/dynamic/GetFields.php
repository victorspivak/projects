<?php

class AbstractEntity
{
	protected $id;

	function __construct($props = array())
	{
		$this->setProperties($props);
	}

	function __get($name)
	{
		echo "get $name\n";
		return $this->{$name};
	}

	function __set($name, $value)
	{
		echo "set $name with $value\n";
		$this->{$name} = $value;
	}

	public function __call($method, $args)
	{
		$verb = substr($method, 0, 3);
		switch($verb)
		{
			case "get":
				$prop=$this->get_property_name(substr($method, 3));
				return $this->{$prop};
			case "set":
				$prop=$this->get_property_name(substr($method, 3));
				$this->{$prop} = $args[0];
				return null;
			default:
				throw new Exception("The \'" . $method . "(" . implode(" , ", $args) . ")\' not found");
		}
	}

	public function setProperties(array $props)
	{
		foreach($props as $name => $value)
		{
			$this->{$name} = $value;
		}
	}

	public function toString()
	{
		$fields = get_object_vars($this);
		$str = "";

		foreach($fields as $name => $value)
		{
			if (isset($value)) $str = "$str $name  =>  $value\n";
		}

		return $str;
	}

	private function get_property_name($property)
	{
		return strtolower(substr($property, 0, 1)) . substr($property, 1);
	}
}

class Item extends AbstractEntity
{
	public $name;
	public $description;
	public $parent;
	public $created;
	public $updated;
	public $deleted;
}

class File extends Item
{
	public $size;
	public $upload_user_name;
}

$file = new File(array(
	'id' => "100000",
	'name' => "filename",
	'size' => 9999,
));

echo $file->toString();

?>
