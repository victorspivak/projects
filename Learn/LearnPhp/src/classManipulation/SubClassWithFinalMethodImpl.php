<?php
/**
 * Created by IntelliJ IDEA.
 * User: victor
 * Date: 3/14/14
 * Time: 3:01 AM
 */

class SubClassWithFinalMethodImpl extends ClassWithFinalMethodImpl {
    public final function foo()
    {
        echo "Sub Class\n";
    }
} 