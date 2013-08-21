
context('xApply')

test_that('xApply', {

	expect_equal( xApply(identity, list(1)), 1 )
	expect_equal(
		xApply( function (...) list(...), list(1,2,3) ),
		 list(1,2,3) )
	expect_equal(
		xApply( function (a, ...) list(a, ...), list(1,2,3) ),
		list(1,2,3) )

	expect_error( xApply( function () Null , list(1) ))
	expect_error( xApply( function (a) Null , list(1, 2) ))

})

test_that('lazy-evaluation doesnt interfere with call evaluation', {

	f = function (a) {
		( function (b) {

			xApply( function (c) c + c, list(b) )

		} )(a)
	}
	expect_equal(f(10), 20)
})