
context("xFixDefaults")

test_that("xFixDefaults", {

	expect_equal(
		xFixDefaults( function (a = 1) a )(),
		1)																															

	expect_equal(
		xFixDefaults( function (a, b=a) b )(1),
		1
	)

	expect_equal(xArity(xFixDefaults(Reduce)), 3)

})
