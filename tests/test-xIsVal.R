
message("xIsVal")

lock <- function (name) {

	name <- match.call()$name

	parent_frame <- parent.frame()
	lockBinding(name, parent_frame)
}

unlock <- function (name) {

	name <- match.call()$name

	parent_frame <- parent.frame()
	unlockBinding(name, parent_frame)
}

test_that("xIsVal", {


	a <- 1
	lock(a)

	expect_true(xIsVal(a))
	unlock(a)
	expect_false(xIsVal(a))

})
