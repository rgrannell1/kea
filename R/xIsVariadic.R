
#' xIsVariadic
#'
#' Is a function variadic?
#'
#' @param
#'    fn an arbitrary function or primitive function.
#'
#' @return
#'    a boolean value.
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



	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	"..." %in% xParams(fn)

}
