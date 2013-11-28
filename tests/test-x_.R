
message("x_ monad laws")

test_that("Type constructor doesn't screw with data (left identity)", {

	expect_equal(x_(10)$x(), 10)
	expect_equal(x_(function() 1)$x()(), 1)

})

test_that("Type constructor flattens nested x_() inputs (right identity)", {

	expect_equal(x_(x_(10))$x(), 10)
	expect_equal(x_(x_(function() 1))$x()(), 1)

})

message("x_ methods")

test_that('method chaining works as expected, for some methods.', {

	expect_that(
		x_(function (a, b) a + b)$
		xArity()$
		x(),
		equals(2)
	)

	expect_that(
		x_(list(1, 2, 3))$
		xMap( function (x) x + 1 )$
		x(),
		equals(list(2, 3, 4)) )

})
