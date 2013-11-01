
message("xIsVariadic")

test_that("xIsVariadic", {

	expect_equal(xIsVariadic(function (x) x), False)
	expect_equal(xIsVariadic(function () {}), False)
	expect_equal(xIsVariadic(function (...) x), True)
	expect_equal(xIsVariadic(function (x, ...) x), True)

})
