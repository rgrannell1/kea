
context("xInsertBy")

test_that("xInsertBy", {

	expect_that(
		xInsertBy("<", 3, c(1, 5, 6)),
		equals(list(1, 3, 5, 6)) )

})
