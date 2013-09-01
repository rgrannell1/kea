
context("xJuxtapose")

test_that("xJuxtapose", {
	
	expect_that(
		xJuxtapose(list(identity, function (x) x + 1, function (x) x + 2))(1), 
		equals(list(1, 2, 3)) )

	expect_that(
		xJuxtapose(list())(1), 
		equals(list()) )

})

