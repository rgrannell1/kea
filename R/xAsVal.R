
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
	pframe <- parent.frame()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))

	str <- toString(match.call()$str)

	assert(
		exists(str, envir = pframe),
		exclaim$variable_non_existent(str))

	assert(
		(is.character(str) && length(str) == 1) ||
		is.name(str), parent_call)

	lockBinding(str, pframe)
}
