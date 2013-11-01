
message("xFlatten")

test_that("xFlatten", {

	nested <- list(
		list(
			2,
			list(3, 4)),
		list(
			5, 6, 7),
		list(
			8, 9, list(list(10)) ),
		1
	)

	expect_that(xFlatten(Inf, nested), equals(nested))
	expect_that(
		xFlatten(1, nested),
		equals( as.list(unlist(nested)) ))

	expect_that(
		xFlatten(2, nested),
		equals(list(
			list(2, 3, 4),
			list(5, 6, 7),
			list(8, 9, 10),
			1
		))
	)

})
