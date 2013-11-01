
message("xUntil")

test_that("xUntil", {

	expect_that(
		xUntil(
			function (x) x == 100,
			function (x) x + 1,
			0
		),
		equals(100)
	)

})
