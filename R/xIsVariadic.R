
#' xIsVariadic
#'
#' Does a function have ellipsis arguments?
#'
#' @section Type Signature:
#'     any -> &lt;logical>
#'
#' @param
#'    fn an function. The function to test for
#'    variadic parametres
#'
#' @return
#'    A boolean value.
#'
#' @family parametre_functions
#'
#' @example
#'    inst/examples/example-xIsVariadic.R
#'
#' @rdname xIsVariadic
#' @export

xIsVariadic <- MakeFun(function (fn) {

	params <- xParamsOf(fn)

	if (length(params) == 0) {
		logical(0)
	} else {
		"..." %in% xParamsOf(fn)
	}
})
