
context("xTmap")

test_that("xTmap", {

	expect_equal(xTmap(identity, list()), list())
	expect_equal(
		xTmap( identity, list(1, list(2, 3)) ),
		list(1, list(2, 3)) )
	expect_equal(
		xTmap( identity, list(1, list(2, 3)) ),
		list(1, list(2, 3)) )
	expect_equal(
		xTmap( paste0, list(1, list(2, 3)) ),
		list('1', list('2', '3')) )

})
