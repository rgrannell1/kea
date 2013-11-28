
message("xAssoc")

test_that("xAssoc", {

	expect_equal(
		xAssoc( list(list('a', 1), list('b', 2)) ),
		list(a = 1, b = 2))

	expect_equal(
		xAssoc( list(list('ab', list(1, 2)), list('b', 2)) ),
		list(ab = list(1, 2), b = 2))

})