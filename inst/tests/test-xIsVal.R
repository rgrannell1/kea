
context("xIsVal")

lock <- function (name) {
	
	name <- match.call()$name

	pframe <- parent.frame()
	lockBinding(name, pframe)
}

unlock <- function (name) {
	
	name <- match.call()$name

	pframe <- parent.frame()
	unlockBinding(name, pframe)
}

test_that("xIsVal", {


	a <- 1
	lock(a)

	expect_true(xIsVal(a))
	unlock(a)
	expect_false(xIsVal(a))

})
