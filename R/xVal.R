
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
#'    \code{the_letter_a <- "a"}
#'    \code{xVal(the_letter_a, "a")}
#'
#'    As the above call shows \code{xVal} is a standard function,
#'    not an infix function like normal assignment.
#'
#' @param
#'    sym a symbol or string. The variable name
#'    to assign a value.
#'
#' @param
#'    val an arbitrary value. The value to be assigned.
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
#' @example
#'    inst/examples/example-xVal.R
#'
#' @rdname xVal
#' @export

xVal <- function (sym, val) {
	# assign a constant value to the calling environment.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	insist $ must_not_be_missing(sym)
	insist $ must_not_be_missing(val)

	sym <- toString(match.call()$sym)
	insist $ must_be_collection(sym, invoking_call)
	sym <- unit_to_value(as_atom(sym, 'character'))

	insist $ must_be_unlocked(sym, parent_frame, invoking_call)

	assign(sym, val, envir = parent_frame)
	lockBinding(sym, parent_frame)
}
