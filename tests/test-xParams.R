
message("xParams")

test_that("xParams", {

	expect_equal(xParams(function () {}), character(0))
	expect_equal(xParams(function (a) a), "a")
	expect_equal(xParams(function (a = 1, b) a), c("a", "b"))

})
