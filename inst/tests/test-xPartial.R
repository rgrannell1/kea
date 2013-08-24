
context('xPartial')

test_that('xPartial', {

	expect_equal(
		xPartial(function (a, b, c) a+b+c, list(a = 1,b = 2))(3),
		6
	)
	expect_equal(
		xPartial(Reduce, list(f = "+", right = TRUE))(1:10),
		55)
	expect_equal(
		xParameters(xPartial( function (a, b, c){}, list(b =1) )),
		c('a', 'c')
	)

})
