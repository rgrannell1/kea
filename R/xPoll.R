
#' xPoll
#'
#' Count the number of times a function returns
#' true when mapped over a collection.
#'
#' @param
#'    pred a unary predicate function.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a non-negative whole number.
#'
#' @family higher_order_functions
#'
#' @family collection_functions
#'
#' @template
#'    Variadic
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

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(
			pred, profile_object(pred)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	pred <- match.fun(pred)

	if (length(coll) == 0) {
		integer(0)
	} else {
		count <- 0
		for (ith in seq_along(coll)) {

			is_match <- try_higher_order(
				pred( coll[[ith]] ),
				invoking_call)

			assert(
				is.logical(is_match), invoking_call)

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
