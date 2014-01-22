
#' xDropWhile
#'
#' Take every element in a collection from the first time a predicate
#' is false or na until the end of the collection.
#'
#' @param
#'    pred a unary predicate.
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
#'	  Returns the emty list if \code{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xDropWhile
#' @export

xDropWhile <- function (pred, coll) {
	# (any -> logical) -> Collection any -> [any]
	# take every element from the first element for which
	# pred is false to the end of coll

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parametre_missing(pred))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_fn_matchable(pred, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ith <- 1

		for (ith in seq_along(coll)) {

			is_match <- try_hof(
				pred( coll[[ith]] ),
				invoking_call)

			insist$is_logical_result(is_match, pred, invoking_call)

			if (!isTRUE(is_match)) {
				return (as.list( tail(coll, length(coll) - (ith - 1)) ))
			}
		}
		list()
	}
}

#' @rdname xDropWhile
#' @export

xDropWhile... <- function (pred, ...) {
	xDropWhile(pred, list(...))
}