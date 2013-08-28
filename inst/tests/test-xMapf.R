
context("xFmap")

test_that("xFmap", {

  expect_that( xFmap(identity)(list()), equals(list()) )  
  expect_that( xFmap(identity)(c(1:3)), equals(list(1, 2, 3)) )
  expect_that( xFmap(function (x) x^2)(c(1:3)), equals(list(1, 4, 9)) )
})
