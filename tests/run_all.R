
require(testthat)

Null <- NULL
Na <- NA
True <- TRUE
False <- FALSE

Truth <- 
	function (...) True
Falsity <- 
	function (...) False

test_package('arrow')
