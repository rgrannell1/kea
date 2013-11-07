
message("xAsVar")

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

test_that("xAsVar", {

	a <- 1
	lock(a)

	xAsVar(a)
	expect_false(bindingIsLocked("a", environment()))

	xAsVar(non_existent)

})

