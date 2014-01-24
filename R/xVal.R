
#' xVal
#'
#' Assign a constant value to the calling environment.
#'
#' @details
#'    xVal works like normal R assignment, with the exception
#'    that if any attempt to update the assigned variable
#'    will result in an error. This is analogous to const in
#'    other languages.
#'
#' @param
#'    sym a symbol or string.
#'
#' @param
#'    val an arbitrary value.
#'
#' @return
#'    Null; this function is used for side-effects.
#'
#' @section Corner Cases:
#'    Overwrites the value referenced by \code{sym}r if the variable
#'    already exists in the parent frame.
#'
#' @family immutable_value_functions
#'
#' @rdname xVal
#' @export

xVal <- function (sym, val) {
	# assign a constant value to the calling environment.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(sym), invoking_call,
		exclaim$parametre_missing(sym))

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	sym <- toString(match.call()$sym)
	insist$must_be_collection(sym, invoking_call)
	sym <- to_value_unit(as_typed_vector(sym, 'character'))

	insist$must_be_of_length(sym, 1, invoking_call)
	insist$must_be_unlocked(sym, parent_frame, invoking_call)

	assign(sym, val, envir = parent_frame)
	lockBinding(sym, parent_frame)
}

