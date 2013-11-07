
message("xAsVal")

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

test_that("xAsVal", {

	a <- 1

	xAsVal(a)
	expect_true(bindingIsLocked("a", environment()))

	unlock(a)
	expect_false(bindingIsLocked("a", environment()))

})

