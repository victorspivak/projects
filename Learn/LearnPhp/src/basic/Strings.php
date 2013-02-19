<?php
function println($str) {print $str . "\n";}

$s1 = "Hello World of PHP";
println(strlen($s1));
println(strrev($s1));
println($s1);
println(strpos($s1, "World"));
println(strpos($s1, "World", 7));
println(substr($s1, 6, 5));
println(strtolower($s1));




?>
