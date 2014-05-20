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

function underscoreToCamelCase( $string, $first_char_caps = false)
{
    if( $first_char_caps == true )
    {
        $string[0] = strtoupper($string[0]);
    }
    $func = create_function('$c', 'return strtoupper($c[1]);');
    return preg_replace_callback('/_([a-z])/', $func, $string);
}

println(underscoreToCamelCase('hello_world', true));

?>
