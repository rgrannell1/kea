
context("xMapMany")

test_that("xMapMany", {
  
	expect_that( xMapMany(identity, 1:3), equals(list(1, 2, 3)) )
	expect_that( xMapMany("+", 1:3, 1:3), equals(list(2, 4, 6)) )
	expect_that( xMapMany("+", 1:3, 1), equals(list(2, 3, 4)) )

})
