
#' xAsVal
#'
#' Convert a normal R variable to a constant value.
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

xAsVal <- function (str) {

	parent_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- toString(match.call()$str)
	str <- coerce_to_typed_vector(str, "character", True)

	assert(
		length(str) == 1, parent_call,
		exclaim$must_have_length(str, 1))

	assert(
		exists(str, envir = parent_frame),
		exclaim$variable_non_existent(str))

	lockBinding(str, parent_frame)
}
