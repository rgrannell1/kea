
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
#'    returns the empty list if \code{coll} is length-zero.
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

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	fn <- match_fn(fn)

	function (coll) {
		"a function created by xVectorise."
		""
		invoking_call <- sys.call()

		try_higher_order(
			xMap(fn, coll), invoking_call)
	}
}

#' @rdname xVectorise
#' @export
# for my american friends.

xVectorize <- xVectorise
