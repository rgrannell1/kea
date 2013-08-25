
context("xAnd")

test_that("xAnd", {

	expect_true(xAnd(Truth, Truth)(1))
	expect_false(xAnd(Truth, Falsity)(1))
	expect_false(xAnd(Falsity, Truth)(1))
	expect_false(xAnd(Falsity, Falsity)(1))

	# assume NA is implemented correctly
	# if this much works
})
