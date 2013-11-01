
message("xAsVal")

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

test_that("xAsVal", {

	a <- 1

	xAsVal(a)
	expect_true(bindingIsLocked("a", environment()))

	unlock(a)
	expect_false(bindingIsLocked("a", environment()))

})

