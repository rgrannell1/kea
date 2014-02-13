
#' xSelect
#'
#' Include all elements from a collection matching a predicate.
#'
#' @details
#'    \bold{xSelect} applies a predicate function
#'    such as \bold{is.integer} or \bold{is.null} to each element
#'    of a collection. The elements for which the predicate
#'    returned true are kept, and the remaining elements are
#'    removed from the collection.
#'
#' @param
#'    pred a predicate. The function used to test each element of
#'    the input collection.
#'
#' @param
#'    coll a collection. The collection to remove elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero,
#'    or no match is found. If the predicate returns a
#'    non-logical value an error is thrown. If an na value
#'    is returned by the predicate it is treated as a false value.
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSelect.R
#'
#' @rdname xSelect
#' @export

xSelect <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# returns coll[i] such that
	# pred(coll[i]) is true

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parametre_missing(pred))

	insist $ must_not_be_missing(coll)

	insist $ must_be_fn_matchable(pred, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- try_hof(
			vapply(coll, pred, logical(1), USE.NAMES = False),
			invoking_call)

		as.list( coll[ !is.na(ind) & ind ] )
	}
}

#' @rdname xSelect
#' @export

xSelect... <- function (pred, ...) {
	xSelect(pred, list(...))
}
