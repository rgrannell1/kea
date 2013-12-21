
#' xVectorise
#'
#' Partially apply xMap with a function.
#'
#' @param
#'    fn a unary function.
#'
#' @return
#'    a unary function of coll.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    mapping_functions
#'
#' @export

xVectorise <- function (fn) {
	# (any -> any) -> ([any] -> [any])
	# shorthand for partially applying map.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	function (coll) {
		"a function created by xVectorise."
		""
		invoking_call <- sys.call()

		try_higher_order(
			xMap(fn, coll), invoking_call)
	}
}

#' @export
# for my american friends.

xVectorize <- xVectorise
