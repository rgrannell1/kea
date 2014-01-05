
#' xZipWith
#'
#' Generate a list of n-element lists from n collections,
#' and apply a function to each n-element list.
#'
#' @param
#'    fn an n-ary function, or a symbol or
#'    name identifying such a function.
#'
#' @param
#'    colls n-vectors, lists or pairlists.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    returns a list of equal length to the shortest input collection.
#'
#' @section Corner Cases:
#'    the empty list is returned if the shortest collection has
#'    length-zero, or no collections are included.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xZipWith
#' @export

xZipWith <- function (fn, colls) {
	# function -> [any] -> ... -> [[any]]
	# takes n lists/vectors, generates a list of n-tuples.
	# returns the result of applying f to each n-tuple.
	# excess elements are discarded.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		length( unique(sapply(colls, length)) ) == 1,
		invoking_call,
		exclaim$must_be_collection_of_equal_lengths(
			colls, summate(colls)) )

	fn <- match.fun(fn)

	if (length(colls) == 0 || length(colls)[[1]] == 0) {
		list()
	} else {

		unname(do.call( Map, c(list(fn),
			Map(
				function (elem) {
					head( elem, length(colls)[[1]] )
				},
				colls
		)) ))

	}
}

#' @rdname xZipWith
#' @export

xZipWith... <- function (fn, ...) {
	xZipWith(fn, list(...))
}
