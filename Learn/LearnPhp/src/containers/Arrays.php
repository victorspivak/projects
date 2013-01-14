<?php
$ar = array(1,2,3,4,5);

printf("Array length is %d\n", count($ar));
printf("Array second element is %d\n", $ar[2]);

$ar[] = 6;
$ar[] = 7;
$ar[] = 8;

printf("Array length is %d\n", count($ar));
printf("Array seventh element is %d\n", $ar[7]);
print_r($ar);

$v = array_pop($ar);
printf("Array length is %d\n", count($ar));
printf("Popped element is %d\n", $v);
print_r($ar);

$ar1 = array_chunk($ar, 2);
print_r($ar1);

//create map
$ar2 = array(10, 20, 30, 40, 50, 60, 70);
$m1 = array_combine($ar, $ar2);
print_r($m1);

$ar3 = array_fill(5, 10, 9999);
print_r($ar3);
printf("%d\n", $ar3[1]);

if (!isset($ar3[1]))
    printf("It is not set\n");

function odd($var)
{
    return($var & 1);
}

function even($var)
{
    return(!($var & 1));
}

$array1 = array("a"=>1, "b"=>2, "c"=>3, "d"=>4, "e"=>5);
$array2 = array(6, 7, 8, 9, 10, 11, 12);

echo "Odd :\n";
print_r(array_filter($array1, "odd"));
echo "Even:\n";
print_r(array_filter($array2, "even"));

print_r(array_keys($array1));
print_r(array_keys($array2));

foreach($ar as $v)
    print "(" . $v . ")\n";

foreach($array1 as $v)
    print "(" . $v . ")\n";

?>

