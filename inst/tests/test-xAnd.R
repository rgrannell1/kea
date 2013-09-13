
context("xAndLift")

test_that("xAndLift", {

	expect_true(xAndLift(Truth, Truth)(1))
	expect_false(xAndLift(Truth, Falsity)(1))
	expect_false(xAndLift(Falsity, Truth)(1))
	expect_false(xAndLift(Falsity, Falsity)(1))

	# assume NA is implemented correctly
	# if this much works
})
