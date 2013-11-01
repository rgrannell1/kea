
message("xFixDefs")

test_that("xFixDefs", {

	expect_equal(
		xFixDefs( function (a = 1) a )(),
		1)

	expect_equal(
		xFixDefs( function (a, b=a) b )(1),
		1
	)

	expect_equal(xArity(xFixDefs(Reduce)), 3)

})
