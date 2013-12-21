
#' xAsVal
#'
#' Convert a normal R variable to a constant value.
#'
#' @param str a string or symbol.
#'
#' @return Null; used for side-effect.
#'
#' @section Corner Cases:
#'    throws an error if attempting to convert a
#'    variable that doesn't exist (in the parent frame).
#'
#' @export

xAsVal <- function (str) {
	# lock the binding for an R variable.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	str <- toString(match.call()$str)
	str <- as_typed_vector(str, "character", True)

	assert(
		length(str) == 1, invoking_call,
		exclaim$must_have_length(str, 1))

	assert(
		exists(str, envir = parent_frame),
		exclaim$variable_non_existent(str))

	lockBinding(str, parent_frame)
}
