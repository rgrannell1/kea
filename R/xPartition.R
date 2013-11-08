
#' xPartition
#'
#' Divide elements in a collection into lists based on an equivelence predicate.
#'
#' @param pred a binary predicate function.
#' @param coll a collection
#'
#' @return a list of lists.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xPartition <- function (pred, coll) {
	# partition a set into an equivalence class.

	parent_call <- sys.call()

	assert(
		!missing(pred), parent_call,
		exclaim$parameter_missing(pred))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	pred <- dearrowise(pred)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(pred), parent_call,
		exclaim$must_be_matchable(pred))

	pred <- match_fn(pred)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

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
				if (pred( elem, parts[[jth]][[1]] )) {
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
