
message("xWhile")

test_that("xWhile", {

	expect_that(
		xWhile(
			function (x) x != 100,
			function (x) x + 1,
			0
		),
		equals(100)
	)

})
