
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

xSplitBy <- function (pred, coll) {

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

		bisect <- function (coll) {
			# split a collection into a head and tail
			# using a predicate.

			for (ith in 1:(length(coll) - 1)) {

				is_match <- try_hof(
					pred( coll[[ith]], coll[[ith + 1]] ),
					invoking_call)

				if (isTRUE(is_match)) {

					return (
						list(
							head(coll, ith),
							tail(coll, -ith)) )
				}
			}

			list(coll, list())
		}

		cleaved <- list()
		cleaved_current <- 1

		while (length(coll) > 0) {

			trimmed <- bisect(coll)
			cleaved[cleaved_current] <- list(as.list(trimmed[1]))

			coll <- trimmed[[2]]
			cleaved_current <- cleaved_current + 1
		}

		cleaved
	}
}

#' @rdname xSplitWith
#' @export

xSplitBy... <- function (pred, ...) {
	xSplitBy(pred, list(...))
}

