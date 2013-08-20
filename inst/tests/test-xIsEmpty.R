
context('xIsEmpty')

test_that('xIsEmpty', {

	expect_equal(xIsEmpty(Null), True)
	expect_equal(xIsEmpty(list()), True)
	expect_equal(xIsEmpty(integer(0)), True)

	expect_equal(xIsEmpty(1), False)
	expect_equal(xIsEmpty(c(1, 2)), False)
})