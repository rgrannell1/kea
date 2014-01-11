
#' xMapIndexed
#'
#' Apply a binary function to each element of a
#' collection and its index.
#'
#' @param
#'    fn a binary function.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list is \code{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xMapIndexed
#' @export

xMapIndexed <- function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]
	# Map over a collection, also passing each elements
	# index.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {

		Map(
			function (ind) {

				try_higher_order(
					fn( coll[[ind]], ind ),
					invoking_call)
			},
			seq_along(coll)
		)
	}
}

#' @rdname xMapIndexed
#' @export

xMapIndexed... <- function (fn, ...) {
	xMapIndexed(fn, list(...))
}
