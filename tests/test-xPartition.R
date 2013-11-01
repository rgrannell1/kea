
message("xPartition")

test_that("xPartition", {

	expect_that(
		xPartition("==", list()),
		equals(list())
	)

	expect_that(
		xPartition("==", list(1, 1, 2, 2, 3, 4)),
		equals( list(
			list(1, 1),
			list(2, 2),
			list(3), list(4))
	))

	expect_that(
		xPartition(function (x, y) x == 1, list(1, 2, 3, 4)),
		equals( list(
			list(1), list(2),
			list(3), list(4) )) )
})

