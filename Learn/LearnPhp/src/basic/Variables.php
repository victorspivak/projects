<?php
$a = "Hello";
$a = 5;

printf ("Value %d\n", $a);

$a1 = $a?:10;
printf ("Value %d\n", $a1);

$a1 = $aaa?:10;
printf ("Value %d\n", $a1);
?>