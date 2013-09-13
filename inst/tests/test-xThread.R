
context("xThread")

test_that("xThread", {
  
	expect_that('=>'(1), equals(1))

	expect_that(
  		'=>'(
	  		10,
	  		function (x) x + 1,
	  		function (x) x + 2
	  	),
	  	equals(13))

})
