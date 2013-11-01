
message("xZipWith")

test_that("xZipWith", {

	expect_that(
		xZipWith("+", 1:10, list()),
		equals(list())
	)

	expect_that(
		xZipWith("+", 1:3, 1:3),
		equals(list(2, 4, 6))
	)

	expect_that(
		xZipWith(function (...) list(...), 1:3, 1:3),
		equals(
			list(list(1, 1), list(2, 2), list(3, 3)) ))

	expect_that(
		xZipWith(function (...) list(...), 1:10, 1:3, 1),
		equals( list(list(1L, 1L, 1)) )
	)

	expect_that(
		xZipWith(function (x) x * x, 1:3),
		equals(list(1, 4, 9))
	)

})
