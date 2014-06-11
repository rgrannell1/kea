
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


	fn <- match_fn(fn)

	isTRUE("..." %in% xParamsOf(fn))
})
