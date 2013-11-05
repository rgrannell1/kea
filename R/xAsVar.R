
xAsVar <- function (str) {
	# unlock a constant binding

	parent_call <- sys.call()
	pframe <- parent.frame()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- toString(match.call()$str)

	assert(
		(is.character(str) && length(str) == 1) ||
		is.name(str), parent_call,
		exclaim$must_be_nameable(str))

	if (exists(str, envir = pframe)) {
		unlockBinding(str, pframe)
	}
}
