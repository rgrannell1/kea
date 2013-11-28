
message("xPluck")

test_that("xPluck", {

	expect_equal(
		xPluck('', list( list(a = 1) )),
		list(list()) )

	expect_equal(
		xPluck('a', list( list(a = 1), list(a = 2), pairlist(a = 4) )),
		list(list(1), list(2), list(4))
	)

	expect_equal(
		xPluck('a', list( list(a = 1, a = 2), list(a = 2), list(a = 40) )),
		list(list(1), list(2), list(40))
	)

})
