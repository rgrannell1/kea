
message("xFlatMap")

test_that("xFlatMap", {

	expect_that(
		xFlatMap(identity, list()),
		equals(list())
	)

	expect_that(
		xFlatMap(function (x) list(x+1), 1:3),
		equals(list(2, 3, 4))
	)
})
