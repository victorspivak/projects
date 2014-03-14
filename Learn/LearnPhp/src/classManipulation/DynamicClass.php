<?php
function __autoload($class)  {
    $code = "class $class {public function run() {echo '$class';}" .
    'public function __call($name,$args) {
            $args=implode(",",$args);
            echo "$name ($args)<br>";
        }
    }';

    eval($code);
}

$app=new Klasse();
$app->run();
$app->HelloWorld();

?>
