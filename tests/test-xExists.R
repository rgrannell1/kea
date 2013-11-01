
message("xExists")

test_that("xExists", {

	expect_equal(xExists(identity), False)
	expect_equal(xExists(function (x) x == x^2, 1:10), True)
	expect_equal(xExists(function (x, y) x+y != y+x, 1:10, 1:10), False)

})
