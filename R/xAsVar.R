
xAsVar <- function (str) {
	# unlock a constant binding
	
	pcall <- sys.call()
	pframe <- parent.frame()

	str <- toString(match.call()$str)

	assert(
		(is.character(str) && length(str) == 1) || 
		is.name(str), pcall)

	if (exists(str, envir = pframe)) {
		unlockBinding(str, pframe)
	}
}
