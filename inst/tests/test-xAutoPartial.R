
context("xAutoPartial")

test_that("xAutoPartial", {

	expect_that(
		xAutoPartial( function (a, b, c) a + b + c )(1)(2, 3),
		equals(6)
	)
	expect_that(
		xAutoPartial( function (a, b, c) a + b + c )(1)()(2)(3),
		equals(6)
	)
	expect_that(
		xAutoPartial( function (a, b, c) a + b + c )()(1, 2, 3),
		equals(6)
	)
})

test_that("lazy semantics are intact", {

	expect_error(
		xAutoPartial( identity )(x + 1)
	)
	
	bod <- xAutoPartial(
		function (x, y) {
			as.list(match.call())[-1]$x
		}
	)(x + 1, y^2)
	
	f <- function () {}
	body(f) <- bod

	expect_equal(f(1), 2)

})
