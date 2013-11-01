
message('xFormals')

test_that('xFormals', {

	expect_equal( xFormals(function () {}), list() )
	expect_equal( xFormals(function (a) {}), list(a = quote(expr=)) )
	expect_equal( xFormals(function (a = 1, b) {}), list(a = 1, b = quote(expr=)) )

	expect_equal( xFormals(function (...) {} ), list("..." = quote(expr=)) )

	expect_equal( xFormals('+'), list(e1 = quote(expr=), e2 = quote(expr=)) )

})
