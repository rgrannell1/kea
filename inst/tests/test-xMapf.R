
context("xMapf")

test_that("xMapf", {

  expect_that( xMapf(identity)(list()), equals(list()) )  
  expect_that( xMapf(identity)(c(1:3)), equals(list(1, 2, 3)) )
  expect_that( xMapf(function (x) x^2)(c(1:3)), equals(list(1, 4, 9)) )
})
