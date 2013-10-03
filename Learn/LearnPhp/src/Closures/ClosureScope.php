<?php
$result = 0;

$f1 = function () use ($result) {
    $result++;
    var_dump($result);
};

$f2 = function () use (&$result) {
    $result++;
    var_dump($result);
};

$f3 = function () use (&$result) {
    $result++;
    var_dump($result);
};

$result++;

$f1();
$f1();
$f1();
$f2();
$f2();
$f2();
$f3();
$f3();
$f3();

?>
