
require(needy)
require(testthat)

Truth <- 
	function (...) True
Falsity <- 
	function (...) False

test_package('arrow')
