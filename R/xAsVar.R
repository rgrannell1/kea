xAsVar <- function (str) {
	# unlock a constant binding

	parent_call <- sys.call()
	pframe <- parent.frame()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- toString(match.call()$str)

	assert(
		length(str) == 1, parent_call,
		exclaim$must_have_length(str, 1))

	if (exists(str, envir = pframe)) {
		unlockBinding(str, pframe)
	}
}
