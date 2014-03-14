<?php
/**
 * Created by IntelliJ IDEA.
 * User: victor
 * Date: 1/3/14
 * Time: 6:15 PM
 */

namespace address;

class Address {
    private $street;
    private $city;
    private $state;
    private $zip;

    function __construct($street, $city, $state, $zip)
    {
        $this->city = $city;
        $this->state = $state;
        $this->street = $street;
        $this->zip = $zip;
    }

    /**
     * @return mixed
     */
    public function getCity()
    {
        return $this->city;
    }

    /**
     * @return mixed
     */
    public function getState()
    {
        return $this->state;
    }

    /**
     * @return mixed
     */
    public function getStreet()
    {
        return $this->street;
    }

    /**
     * @return mixed
     */
    public function getZip()
    {
        return $this->zip;
    }
}