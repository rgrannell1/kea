
context("xDissoc")

test_that("xDissoc", {

	expect_equal(xDissoc(list()), list())
	
	expect_that(xDissoc(list(a = 1, b = 2, c = list(3), 4)), 
		equals(list(
			list('a', 1),
			list('b', 2),
			list('c', list(3)),
			list('', 4)
		)) )

})
