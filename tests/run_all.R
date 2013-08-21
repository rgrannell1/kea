
require(needy)
require(testthat)

Na <- NA
Null <- NULL
True <- TRUE
False <- FALSE

Truth <- function (x) True
Falsity <- function (x) False
NonApplicabity <- function (x) Na

test_package('arrow')
