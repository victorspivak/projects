<?php

$obj = new SubClassWithFinalMethodImpl();
$obj->foo();








































































function __autoload($class)  {
    $code = file_get_contents($class.'.php');
    $code = str_replace('<?php', '', $code);
    $code = str_replace(' final ', ' ', $code);
    eval($code);
}

