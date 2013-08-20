
context('xUnit')

test_that('xUnit returns the expected unit', {

	expect_equal(xUnit( list(1) ), list())
	expect_equal(xUnit( pairlist(1) ), NULL)
	expect_equal(xUnit( character(1) ), character(0))
	expect_equal(xUnit( 1L ), integer(0))

})

test_that('xUnit operates on listy values only', {
	expect_error(xUnit( matrix(1:4, 2, 2) ))
})
