
#' xSelect
#'
#' Include all elements from a collection matching a predicate.
#'
#' @section Uses:
#'    The select function is useful for taking a collection,
#'    and returning values that meet certain criteria. Likely
#'    uses include selecting rows in a (converted) data frame
#'    that contain a certain value, selecting strings that
#'    match a regular expression, or selecting records
#'    based on a value in a particular field.
#'
#' @param
#'    pred a predicate.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero, or no match is found.
#'
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
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

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(
			pred, summate(pred)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- try_higher_order(
			vapply(coll, pred, logical(1), USE.NAMES = False),
			invoking_call)

		as.list( coll[ !is.na(ind) & ind ] )
	}
}

#' @rdname xSelect
#' @export

xSelect... <- function (pred, ...) {
	do.call(xSelect, list(pred, list(...)))
}
