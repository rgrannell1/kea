
message("xAsVar")

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

test_that("xAsVar", {

	a <- 1
	lock(a)

	xAsVar(a)
	expect_false(bindingIsLocked("a", environment()))

	xAsVar(non_existent)

})

