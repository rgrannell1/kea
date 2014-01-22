
#' xVectorise
#'
#' Convert a function on one value to a function on collections of values.
#'
#' @details
#'    \code{xVectorise} is a convenience function that partially
#'    applies \code{xMap} with a function, creating a vectorised version
#'    of that function.
#'
#' @param
#'    fn a unary function.
#'
#' @return
#'    A unary function of coll.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @family function_modifying_functions
#'
#' @rdname xVectorise
#' @export

xVectorise <- function (fn) {
	# (any -> any) -> ([any] -> [any])
	# shorthand for partially applying map.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	insist$must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)

	function (coll) {
		"a function created by xVectorise."
		""
		invoking_call <- sys.call()

		try_hof(
			xMap(fn, coll), invoking_call)
	}
}

#' @rdname xVectorise
#' @export
# for my american friends.

xVectorize <- xVectorise
