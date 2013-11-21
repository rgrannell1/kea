
message("xFoldListl")

test_that("xFoldListl", {

	expect_that(
		xFoldListl("+", 0, list()),
		equals(0)
	)

	expect_that(
		xFoldListl("+", 0, 1:10),
		equals( as.list(cumsum(0:10)) )
	)

})