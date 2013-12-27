
#' xPartition
#'
#' Divide elements in a collection into lists based on
#' an equivelence predicate.
#'
#' @param
#'    pred a binary predicate function.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a list of lists.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @family higher_order_functions
#'
#' @family collection_functions
#'
#' @export

xPartition <- function (pred, coll) {
	# partition a set into an equivalence class.

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

	pred <- match.fun(pred)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		list( coll[[1]] )
	} else {
		parts <- list( coll[[1]] )
		colls <- coll[ 2:length(coll) ]

		for (ith in 2:length(coll)) {

			elem <- coll[[ith]]
			is_match <- False

			for (jth in seq_along(parts)) {

				is_member <- try_higher_order(
					pred( elem, parts[[jth]][[1]] ),
					invoking_call)

				if (is_member) {

					parts[[jth]] <- c( parts[[jth]], elem )
					is_match <- True
				}
				if (is_match) {
					break
				}
			}
			if (!is_match) {
				parts <- c( parts, list(list(elem)) )
			}

		}
		parts
	}
}

#' @export

xPartition... <- function (pred, ...) {
	xPartition(pred, list(...))
}
