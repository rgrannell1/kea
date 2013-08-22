
context("=>")

test_that("=>", {

	expect_equal( (x %=>% {x + 1})(1), 2 )
	expect_equal( ("x" %=>% {x + 1})(1), 2 )

})
