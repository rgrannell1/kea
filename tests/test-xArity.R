
message('xArity')

test_that('xArity', {

	expect_equal(xArity(function () {}), 0)
	expect_equal(xArity(function (a=1) {}), 1)
	expect_equal(xArity(function (a=1, b=2) {}), 2)

	expect_equal(xArity(function (...) {}), Inf)
	expect_equal(xArity(function (a, b, ...) {}), Inf)

})
