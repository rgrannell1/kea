
context("xPartMap")

test_that("xPartMap", {

  expect_that( xPartMap(identity)(list()), equals(list()) )  
  expect_that( xPartMap(identity)(c(1:3)), equals(list(1, 2, 3)) )
  expect_that( xPartMap(function (x) x^2)(c(1:3)), equals(list(1, 4, 9)) )
})
