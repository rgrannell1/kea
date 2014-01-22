
#' xAsVal
#'
#' Convert a normal R variable to a constant value.
#'
#' @details
#'      xAsVal takes a variable that exists in the
#'      calling environment, and locks it. This prevents
#'      further modification, and if any attempt is made to
#'      modify the variable an error is thrown.
#'
#' @param
#'      sym a symbol or string.
#'
#' @return
#'      Null; used for side-effect.
#'
#' @section Corner Cases:
#'    Throws an error if attempting to convert a
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

	insist$must_be_of_length(sym, 1, invoking_call)
	insist$must_exist(sym, parent_frame, invoking_call)

	lockBinding(sym, parent_frame)
}
