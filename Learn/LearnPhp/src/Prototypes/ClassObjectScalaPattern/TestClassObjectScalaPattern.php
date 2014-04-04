<?php

require_once 'Entity.php';

Prototypes\ClassObjectScalaPattern\Entity::setData();
echo "------------------------------------------- 0\n";
Prototypes\ClassObjectScalaPattern\EntityMapper::instance()->dumpData();

