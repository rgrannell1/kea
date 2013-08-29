
context("xAny")

test_that('xAny', {

	expect_equal(xAny(Truth, list()), logical(0))

	expect_equal(xAny(Truth, 1:3), True)
	expect_equal(xAny(Falsity, 1:3), False)
	expect_true(xAny(function (x) x %% 2 == 0, 1:10))
	expect_equal(xAny(NonApplicability, 1:3), False)
	
})
