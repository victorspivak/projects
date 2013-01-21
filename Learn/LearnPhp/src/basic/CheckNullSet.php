<?php

if (!$v)
    echo "Is not\n";

if (!isset($v))
    echo "Is not set\n";

$v = 0;

if (!$v)
    echo "Is not\n";

if (!isset($v))
    echo "Is not set\n";

$v1 = array(0,0,0,0,0);

if (!$v1[1])
    echo "Is not\n";

if (!isset($v1[1]))
    echo "Is not set\n";

?>
