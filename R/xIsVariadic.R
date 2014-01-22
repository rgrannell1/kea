
#' xIsVariadic
#'
#' Is a function variadic?
#'
#' @param
#'    fn an arbitrary function or primitive function.
#'
#' @return
#'    A boolean value.
#'
#' @family parametre_functions
#'
#' @rdname xIsVariadic
#' @export

xIsVariadic <- function (fn) {
	# function -> Vector boolean
	# is ... in fn's parametres?

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	insist$must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)

	"..." %in% xParamsOf(fn)

}
