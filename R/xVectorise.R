
#' xVectorise
#'
#' Convert a function on one value to a function on collections of values.
#'
#' @details
#'    \bold{xVectorise} is a shorthand function for partially applying
#'    \bold{xMap} with a function, returning a vectorised function.
#'    This can be useful for taking a function that only works on single
#'    values - like \bold{is.null( )} - and extending them to work on
#'    collections of values.
#'
#'    \code{is.nulls <- xVectorise(is.null)}
#'
#'    \code{is.nulls(list(1, 2, Null, 4))}
#'
#'    \code{list(False, False, True, False)}
#'
#' @param
#'    fn a unary function. The function to vectorise.
#'
#' @return
#'    A unary function of coll.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @family function_modifying_functions
#'
#' @example
#'    inst/examples/example-xVectorise.R
#'
#' @rdname xVectorise
#' @export

xVectorise <- function (fn) {
	# (any -> any) -> ([any] -> [any])
	# shorthand for partially applying map.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(fn)
	insist $ must_be_fn_matchable(fn, invoking_call)

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
