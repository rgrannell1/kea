
message("xFoldr")

test_that("xFoldr", {

	expect_that(
		xFoldr("c", 11, 1:10),
		equals(1:11))

	expect_that(
		xFoldr("+", 0, 1:10),
		equals(55))

	expect_that(
		xFoldr("+", 0, list()),
		equals(0)
	)

})
