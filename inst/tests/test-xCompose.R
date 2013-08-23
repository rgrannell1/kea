
context("xCompose")

test_that("xCompose", {

	expect_that(
		xCompose(
			function (x) x,
			function (y) y
		)(1), equals(1))
	expect_that(
		xCompose(
			function (x) 2*x,
			function (x) 3*x
		)(1), equals(6))
	expect_that(
		xCompose(
			function (x) 2*x,
			function (x, y, z, w) x+y+z+w
		)(1, 1, 1, 1), equals(8))

	expect_that(
		xArity(xCompose(
			function (x) 2*x,
			function (x, y, z, w) x+y+z+w
		)), equals(4))

	expect_that(
		names( formals(xCompose(
			function (x) 2*x,
			function (x, y, z, w) x + y + z + w
		)) ), equals(c('x', 'y', 'z', 'w')) )	

})
