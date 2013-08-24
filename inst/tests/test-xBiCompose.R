
compose("xBiCompose")

test_that("xBiCompose", {

	expect_equal
		xBiCompose("+", identity, identity)(10),
		20
	)

})
