
#' xPoll
#'
#' Count the number of times a function returns
#' true when mapped over a collection.
#'
#' @param
#'    pred a unary predicate function. The function with
#'    which to poll each element of the input collection.
#'
#' @param
#'    coll a collection. The collection to poll.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A non-negative whole number.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPoll.R
#'
#' @rdname xPoll
#' @export

xPoll <- function (pred, coll) {
	# (any -> logical) -> Collection any -> integer
	# return the number of elements for which a predicate is true.

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
		integer(0)
	} else {

		count <- 0

		for (ith in seq_along(coll)) {

			is_match <- try_hof(
				pred( coll[[ith]] ), invoking_call)

			insist$must_be_logical_result(is_match, pred, invoking_call)

			if (isTRUE(is_match)) {
				count <- count + 1
			}
		}
		count
	}
}

#' @rdname xPoll
#' @export

xPoll... <- function (pred, ...) {
	xPoll(pred, list(...))
}
