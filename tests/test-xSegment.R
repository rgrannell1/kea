
message("xSegment")

test_that("xSegment", {

	expect_equal(
		xSegment(100, 1:10),
		list(as.list(1:10))
	)
	expect_equal(
		xSegment(1, 1:3),
		list(list(1), list(2), list(3))
	)
	expect_equal(
		xSegment(3, 1:4),
		list(list(1, 2, 3), list(4))
	)
})
