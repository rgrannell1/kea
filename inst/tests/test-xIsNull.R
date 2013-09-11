
context('xIsNull')

test_that('xIsNull', {

	expect_that(xIsNull(Null), equals(True))
	expect_that(
		xIsNull(list(Null, 1, Null, 2)), 
		equals(c(True, False, True, False)) )

})
