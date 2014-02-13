
#' xSplitWith
#'
#' Divide a collection into groups determined by a predicate.
#'
#' @param
#'      pred a predicate.
#'
#' @param
#'      coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A list of lists.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSplitBy.R
#'
#' @rdname xSplitWith
#' @export

xSplitBy <- local({

	bisect <- function (pred, coll, invoking_call) {
		# split a collection into a head and tail
		# using a predicate.

		try_hof({
			for (ith in 1:(length(coll) - 1)) {

				is_match <- pred( coll[[ith]], coll[[ith + 1]] )

				insist $ must_be_logical_result(
					is_match, pred, invoking_call)

				if (isTRUE(is_match)) {

					return (
						list(
							head(coll, ith),
							tail(coll, -ith)) )
				}
			}},
			invoking_call
		)

		list(coll, list())
	}

	function (pred, coll) {

		invoking_call <- sys.call()

		assert(
			!missing(pred), invoking_call,
			exclaim$parametre_missing(pred))

		assert(
			!missing(coll), invoking_call,
			exclaim$parametre_missing(coll))

		insist $ must_be_fn_matchable(pred, invoking_call)
		insist $ must_be_collection(coll, invoking_call)

		pred <- match_fn(pred)

		if (length(coll) == 0) {
			list()
		} else if (length(coll) == 1) {
			as.list(coll)
		} else {

			cleaved <- list()
			cleaved_current <- 1

			while (length(coll) > 0) {

				trimmed <- bisect(pred, coll, invoking_call)
				cleaved[cleaved_current] <- list(as.list(trimmed[1]))

				coll <- trimmed[[2]]
				cleaved_current <- cleaved_current + 1
			}

			cleaved
		}
	}
})

#' @rdname xSplitWith
#' @export

xSplitBy... <- function (pred, ...) {
	xSplitBy(pred, list(...))
}

