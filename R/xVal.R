
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
#'    Kiwi's value functions are built on top of base R's \bold{lockBinding}
#'    mechanism, which is self-described as being 'experimental'.
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
#'    \bold{xVals} overwrites the value referenced by \code{sym} if the variable
#'    already exists in the parent frame.
#'
#' @family immutable_value_functions
#'
#' @example
#'    inst/examples/example-xVal.R
#'
#' @rdname xVal
#' @export

xVal <- MakeFun('xVal', function (sym, val) {

	parent_frame <- parent.frame()

	# -- check if binding is unlocked.

	assign(sym, val, envir = parent_frame)
	lockBinding(sym, parent_frame)

	invisible(Null)
})
