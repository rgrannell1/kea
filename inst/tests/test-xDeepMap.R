
context("xDeepMap")

test_that("xDeepMap", {

	expect_equal(xDeepMap(identity, list()), list())
	expect_equal(
		xDeepMap( identity, list(1, list(2, 3)) ),
		list(1, list(2, 3)) )
	expect_equal(
		xDeepMap( identity, list(1, list(2, 3)) ),
		list(1, list(2, 3)) )
	expect_equal(
		xDeepMap( paste0, list(1, list(2, 3)) ),
		list('1', list('2', '3')) )

})
