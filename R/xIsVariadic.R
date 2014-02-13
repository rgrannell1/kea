
#' xIsVariadic
#'
#' Is a function variadic?
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

xIsVariadic <- function (fn) {
	# function -> Vector boolean
	# is ... in fn's parametres?

	invoking_call <- sys.call()

	insist $ must_not_be_missing(fn)
	insist $ must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)

	"..." %in% xParamsOf(fn)

}
