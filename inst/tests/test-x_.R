
context("x_ monad laws")

test_that("Type constructor doesn't screw with data (left identity)", {

	expect_equal(x_(10)$x(), 10)
	expect_equal(x_(function() 1)$x()(), 1)

})

test_that("Type constructor flattens nested x_() inputs (right identity)", {

	expect_equal(x_(x_(10))$x(), 10)
	expect_equal(x_(x_(function() 1))$x()(), 1)

})

context("x_")

test_that('method chaining works as expected', {

	expect_that(
		x_(function (a, b) a + b)$
		xArity()$
		x(),
		equals(2)
	)

})
