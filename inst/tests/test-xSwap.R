
context("xSwap")

test_that("xSwap", {
  
  expect_that(
  	xSwap(function (a, b) a*b, list(0, Inf) )(1, 2), 
  	equals(2))

  expect_that(
  	xSwap(function (a, b) a*b, list(0, Inf) )(1, 0), 
  	equals(Inf))

  expect_that(
  	xSwap(function (a, b) a*b, list(0, Inf), list(2, -Inf) )(2, 1), 
  	equals(-Inf))

})
