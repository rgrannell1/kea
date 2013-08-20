
context('xInitial')

test_that('xInitial', {

	expect_equal( xInitial(list(1)), list())
	expect_equal( xInitial(list(1, 2)), list(1))
	expect_equal( xInitial(list(1, 2, 3)), list(1, 2))

})
