
context("xVal")

test_that("xVal", {

	xVal(aa, 10)

	expect_equal(aa, 10)
	expect_true( bindingIsLocked('aa', environment()) )

})
