<?php
/**
 * Created by IntelliJ IDEA.
 * User: victor
 * Date: 1/3/14
 * Time: 6:11 PM
 */

namespace hr;

class Person {
    /** @var  string $name */
    private $name;
    /** @var \hr\address\Address  */
    private $address;

    /**
     * @param string $name
     * @param \hr\address\Address $address
     */
    function __construct($name, $address)
    {
        $this->address = $address;
        $this->name = $name;
    }

    /**
     * @return \hr\address\Address
     */
    public function getAddress()
    {
        return $this->address;
    }

    /**
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }
}