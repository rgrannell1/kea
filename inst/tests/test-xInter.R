
context("xInter")

test_that("xInter", {

	expect_equal(xInter(list(), list()), list())
	expect_equal(xInter(list(), list(1)), list())
	expect_equal(xInter(list(1), list()), list())

	expect_equal(
		xInter(list(1, 2), list(1)), 
		list(1))
	
	expect_equal(
		xInter(list(1, 2, 3, 4), list(1, 2)), 
		list(1, 2))
})
