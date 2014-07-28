
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
#' @section Corner Cases:
#'    Returns true when a function has a mix of variadic
#'    and non-variadic parametres.
#'
#' @family parametre_functions
#'
#' @example
#'    inst/examples/example-xIsVariadic.R
#'
#' @rdname xIsVariadic
#' @export

xIsVariadic <- MakeFun('xIsVariadic', function (fn) {

	params <- xParamsOf(fn)

	if (length(params) == 0) {
		logical(0)
	} else {
		"..." %in% xParamsOf(fn)
	}
})
