
message('xNegate')

test_that('xNegate', {

	expect_equal(xNegate(numeric(0)), numeric(0))
	expect_equal(xNegate(c(0, 0)), c(0, 0))
	expect_equal(xNegate(c(1, -1)), c(-1, 1))

})
