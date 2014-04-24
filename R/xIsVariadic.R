
#' xIsVariadic
#'
#' Does a function have ellipsis arguments?
#'
#' @param
#'    fn an function. The function to test for
#'    variadic parametres.
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

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	"..." %in% xParamsOf(fn)

})
