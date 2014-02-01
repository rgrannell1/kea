
#' xMapIndexed
#'
#' Apply a binary function to each element of a
#' collection and its index.
#'
#' @param
#'    fn a binary function. A function that takes a
#'    value in \bold{coll} as its left argument and a
#'    an index as its right argument.
#'
#' @param
#'    coll a collection. The collection to apply a function to.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list is \bold{coll} is length-zero.
#'
#' @family mapping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMapIndexed.R
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

	insist $ must_be_fn_matchable(fn, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {

		Map(
			function (ind) {

				try_hof(
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
