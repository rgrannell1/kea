
#' xAsVar
#'
#' Convert a constant value back into a normal R value.
#'
#' @details
#'     \code{xAsVar} takes a variable in the calling
#'     environmnet, and unlocks it, converting it back
#'     to a normal R variable.
#'
#' @param
#'     sym a string or symbol.
#'
#' @return
#'     Null; used for side-effect.
#'
#' @section Corner Cases:
#'    throws an error if attempting to convert a
#'    variable that doesn't exist (in the parent frame).
#'    Non-locked variables are also allowed.
#'
#' @family immutable_value_functions
#'
#' @example
#'    inst/examples/example-xAsVar.R
#'
#' @rdname xAsVar
#' @export

xAsVar <- function (sym) {
	# unlock a constant binding

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(sym), invoking_call,
		exclaim$parametre_missing(sym))

	sym <- toString(match.call()$sym)

	assert(
		length(sym) == 1, invoking_call,
		exclaim$must_have_length(
			sym, 1, summate(sym)) )

	assert(
		exists(sym, envir = parent_frame),
		exclaim$variable_non_existent(
			sym, summate(sym)) )

	unlockBinding(sym, parent_frame)
}
