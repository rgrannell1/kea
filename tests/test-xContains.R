message("xContains")

test_that("xContains", {

	expect_that( xContains(list(), 1), equals(logical(0)) )
	expect_that(xContains(c(1), 1), equals(True))
	expect_that(xContains(c(1:3), 2L), equals(True))
	expect_that(xContains(c(1:3), 4L), equals(False))

})
