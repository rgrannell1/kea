
#' xSortBy
#'
#' Sort a collection using a predicate function.
#'
#' @details
#'     \bold{xSortBy} allows a collection to be sorted by a custom
#'     comparison operation. A classic example is sorting a collection of collections
#'     (analogous to a data frame) by a particular column.
#'
#'     \code{coll <- list(list('key1', 10), list('key2', 12), list('key3', 0))}
#'
#'     \code{xSortBy((row1 : row2) := xSecondOf(row1) > xSecondOf(row2), coll)}
#'
#'     \code{list(list("key2", 12), list("key1", 10), list("key3", 0))}
#'
#'     In the above example several rows of collection of collections are rearranged
#'     by the value in one of their columns.
#'
#'     \bold{xSortBy} is currently (v0.1.0) inefficient for large collections,
#'     since both recursive and in-place sorting algorithms are not
#'     suitable for use in R.
#'
#' @param
#'    pred a binary predicate. is the left argument \bold{larger} than the
#'    right argument?
#'
#' @param
#'    coll a collection. The collection to sort.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list
#'
#' @section Corner Cases:
#'    If \bold{pred} returns Na it is interpreted as false; the right argument is
#'    larger than the left.
#'
#' @template
#'    Variadic
#'
#' @family reshaping_functions
#'
#' @example
#'    inst/examples/example-xSortBy.R
#'
#' @rdname xSortBy
#' @export

xSortBy <- function (pred, coll) {
	#' (a -> b -> boolean) -> Collection any -> [any]
	#' sort a collection using a comparison function.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(pred)
	insist $ must_not_be_missing(coll)

	insist $ must_be_fn_matchable(pred, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		as.list(coll)
	} else {

		should_swap <- function (index1, index2) {
			isTRUE(pred( coll[[index1]], coll[[index2]] ))
		}

		# insertion sort; a reasonable enough algorithm to use.
		# replace with C++; in-place modification in R is rubbish.
		for (ith in 2:length(coll)) {

			jth <- ith

			try_hof({
				while (jth > 1 && should_swap(jth - 1, jth)) {

					tmp <- coll[[jth - 1]]

					coll[[jth - 1]] <- coll[[jth]]
					coll[[jth]] <- tmp

					jth <- jth - 1
				}},
				invoking_call
			)
		}

		as.list(coll)
	}
}

#' @rdname xSortBy
#' @export

xSortBy... <- function (pred, ...) {
	xSortBy(pred, list(...))
}

