
#' xAsVar
#'
#' Convert a constant value back into a normal R value.
#'
#' @param str a string or symbol.
#'
#' @return Null; used for side-effect.
#'
#' @section Corner Cases:
#'    throws an error if attempting to convert a
#'    variable that doesn't exist (in the parent frame).
#'
#' @family immutable_value_functions
#'
#' @example
#'    inst/examples/example-xAsVar.R
#'
#' @rdname xAsVar
#' @export

xAsVar <- function (str) {
	# unlock a constant binding

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	str <- toString(match.call()$str)

	assert(
		length(str) == 1, invoking_call,
		exclaim$must_have_length(str, 1))

	assert(
		exists(str, envir = parent_frame),
		exclaim$variable_non_existent(str))

	unlockBinding(str, parent_frame)
}
