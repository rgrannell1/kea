
message("xName")

test_that("xName", {

	expect_that(
		xName(character(0), list()),
		equals( structure(list(), names = character(0)) ))

	expect_that( xName('a', 1), equals(list(a = 1)) )
	expect_that(
		xName(c('a', 'b'), list(1, 2)),
		equals(list(a = 1, b = 2)) )

})

