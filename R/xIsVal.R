
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

xIsVal <- MakeFun(function (sym) {
	# symbol | Vector character -> logical
	# is a name binding locked?

	parent_frame <- parent.frame()

	MACRO( Must $ Not_Be_Missing(sym) )

	sym <- match.call()$sym
	MACRO( Must $ Be_Matchable(sym) )

	sym <- toString(sym)

	if (nchar(sym) == 0) {
		False
	} else {
		exists(sym, parent_frame) &&
			bindingIsLocked(sym, parent_frame)
	}
})
