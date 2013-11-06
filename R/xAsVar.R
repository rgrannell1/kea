
#' xAsVar
#'
#' Convert a constant value back into a normal R value.
#'
#' @param str a string or symbol.
#'
#' @return Null; used for side-effect.
#'
#' @section Corner Cases:
#'     throws an error if attempting to convert a variable that doesn't exist (in the parent frame).
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xAsVar <- function (str) {
	# unlock a constant binding

	parent_call <- sys.call()
	pframe <- parent.frame()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- toString(match.call()$str)

	assert(
		length(str) == 1), parent_call,
		exclaim$must_have_length(str, 1))

	assert(
		exists(str, envir = pframe),
		exclaim$variable_non_existent(str))

	if (exists(str, envir = pframe)) {
		unlockBinding(str, pframe)
	}
}
