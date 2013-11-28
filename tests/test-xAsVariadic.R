
message("xAsVariadic")

test_that("xAsVariadic", {

	expect_equal(xArity(xAsVariadic( function (x) x )), +Inf)
	expect_equal(
		xAsVariadic( function (a) a[[1]] + a[[2]] )(1, 2),
		3
	)

})
