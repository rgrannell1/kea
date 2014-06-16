
#' xIsVal
#'
#' Is a variable locked?
#'
#' @section Type Signature:
#'     any -> &lt;logical>
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

	parent_frame <- parent.frame()

	exists(sym, parent_frame) &&
		bindingIsLocked(sym, parent_frame)
})
