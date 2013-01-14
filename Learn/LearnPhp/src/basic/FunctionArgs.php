<?php

function println($str) {print $str . "\n";}

function f1($a) {
    array_pop($a);
}

//passing by reference
function f2(&$a) {
    array_pop($a);
}

$a = array(1,2,3,4,5);
f1($a);
print_r($a);
f2($a);
print_r($a);

function greeting ($name = "Victor") {
    println("Hi $name");
}

greeting();
greeting ("Vadim");

function merge() {
    $arg_count = func_num_args();
    $buf = "";
    foreach(func_get_args() as $arg)
        $buf = $buf . $arg;

    println($buf);
}

merge("a", "b", "c");

?>
