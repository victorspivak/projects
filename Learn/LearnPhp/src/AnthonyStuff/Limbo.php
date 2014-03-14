<?php
require_once 'assertions.php';

class Foo
{
	private $a;

	function __construct($a)
	{
		$this->$a;
	}
}

$m = function ()
{
	//$this->
};

$foo = new Foo(10);
print($foo->as_string());

/** 
* Basics
*/
class Limbo
{

	public function the_truth_is_true()
	{
		assert_true(true); // make this true
	}

	public function dispatch_the_lies()
	{
        $the_sky_is_red = false;
		assert_true($the_sky_is_red == false); // make this false
	}

	public function find_what_is_true___()
	{
		$hells_circles = 9;
		assert_true($hells_circles + 1 == 10);
	}

	public function __and_tell_it_on_the_mount()
	{
		$hells_circles = 9;
		assert_that($hells_circles)->is_equal_to(9);
	}
	
	public function though_you_may_find_truth_is_not_always_easy_to_find()
	{
		$virgil = "the poet";
		assert($virgil == true)->is_equal_to(true);
		assert($virgil === true)->is_equal_to(false);
		assert($virgil)->is_identical_to($virgil);
		
		/*
		* Virgil says: in the PHP inferno, we have a few assert methods we use
		* to establish truth. The is_identical_to() method corresponds to the
		* strict-equal === operator and is_equal_to() corresponds to loose equal == operator. 
		*/
	}
	
	public function tasks_are_actions_to_take()
	{
		// you can delete a task by adding // to the front or just deleting it
		task('understand that tasks are actions you must take');
	}

	public function null_is_nothing()
	{
		$nothing = null;
		assert_that($nothing == '')->is_equal_to(false);
	}

	/**
	* @suppress_warnings
	*/
	public function sometimes_you_will_have_nothing_to_go_by()
	{
		assert_that($undeclared)->is_equal_to(null);
	}
	
	public function if_statements_let_you_check_for_things()
	{
		if (100 < 200)
		{
			$value = 'less';
		}
		else
		{
			$value = 'more';
		}
		assert_that($value)->is_identical_to(__);
	}
	
	/**
	* Exercise I. Finding an apartment in San Francisco is kind of like Limbo. 
	* 
	* -> if the neighborhood is "The Mission", then rent is $2000 per bedroom.
	* -> if the neighborhood is "Russian Hill", then rent is $2500 for the first
	* bedroom and $1000 for each additional room.
	* -> if you don't recognize the neighborhood, it's $1500 per bedroom
	* -> if any of the inputs are invalid, return null.
	*/
	private function get_apartment_prices($neighborhood_name, $bedrooms)
	{
		task('implement this method');
	}
	
	public function try_finding_an_apartment_in_the_mission_on_a_budget()
	{
		assert_that($this->get_apartment_prices("The Mission", 3))->is_equal_to(6000);
	}
	
	public function russian_hill_is_almost_a_nonstarter()
	{
		assert_that($this->get_apartment_prices("Russian Hill", 2))->is_equal_to(3500);
	}
	
	public function a_made_up_neighborhood_should_get_the_default_price()
	{
		assert_that($this->get_apartment_prices("Cow Hollow", 3))->is_equal_to(4500);
	}
	
	public function invalid_input_should_get_a_null_back()
	{
		assert_that($this->get_apartment_prices("The Mission", -1))->is_identical_to(null);
		assert_that($this->get_apartment_prices(null, 2))->is_identical_to(null);
		
		// hint: Are you using == or === ?
		assert_that($this->get_apartment_prices(0, 2))->is_identical_to(null);
	}
}
