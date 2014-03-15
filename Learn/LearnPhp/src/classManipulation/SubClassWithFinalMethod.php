<?php
/**
 * Created by IntelliJ IDEA.
 * User: victor
 * Date: 3/14/14
 * Time: 3:01 AM
 */

class SubClassWithFinalMethod extends ClassWithFinalMethod {
    public function foo()
    {
        echo "Sub Class\n";
		parent::foo();
    }
} 
