
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
	# function -> Vector boolean
	# is ... in fn's parametres?

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(fn) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	"..." %in% xParamsOf(fn)

})
