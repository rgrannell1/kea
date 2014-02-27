
#' xIsVal
#'
#' Is a variable locked?
#'
#' @param
#'    sym a symbol or string. The variable name to test.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If a variable isn't assigned, then \code{false} is returned.
#'
#'
#' @example
#'    inst/examples/example-xIsVal.R
#'
#' @rdname xIsVal
#' @export

xIsVal <- function (sym) {
	# symbol | Vector character -> logical
	# is a name binding locked?

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	insist $ must_not_be_missing_sym(sym)

	sym <- match.call()$sym
	insist $ must_be_matchable(sym, invoking_call)

	sym <- toString(sym)

	if (nchar(sym) == 0) {
		False
	} else {
		exists(sym, parent_frame) &&
			bindingIsLocked(sym, parent_frame)
	}
}
