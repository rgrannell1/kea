
#' xAsVal
#'
#' Convert a normal R variable to a constant value.
#'
#' @param
#'     sym a symbol or string.
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
#'    inst/examples/example-xAsVal.R
#'
#' @rdname xAsVal
#' @export

xAsVal <- function (sym) {
	# lock the binding for an R variable.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(sym), invoking_call,
		exclaim$parametre_missing(sym))

	sym <- toString(match.call()$sym)
	sym <- as_typed_vector(sym, "character", True)

	assert(
		length(sym) == 1, invoking_call,
		exclaim$must_have_length(sym, 1))

	assert(
		exists(sym, envir = parent_frame),
		exclaim$variable_non_existent(sym))

	lockBinding(sym, parent_frame)
}
