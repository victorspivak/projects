<?php

class GrandBase {
	public function inject($o1) {
		print ("GrandBase $o1\n");
	}
}

class Base extends GrandBase {
	public function inject($o1) {
		print ("Base $o1\n");
	}
}

class Child extends Base{
	public function inject($o2, $o3) {
		parent::inject($o2);
		$pClass = get_parent_class($this);
		print ("Parent: $pClass\n");
		$parents = implode(' ', class_parents($this));
		print ("Parents: $parents\n");
		print ("Child $o2 $o3\n");
	}
}

$o = new Child();
$o->inject('1', '2');
