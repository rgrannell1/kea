
require(testthat)

Null <- NULL
Na <- NA
True <- TRUE
False <- FALSE

Mu <- 
	function (...) Na
Truth <- 
	function (...) True
Falsity <- 
	function (...) False

test_package('arrow')
