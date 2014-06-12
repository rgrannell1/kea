
#' xSplitWith
#'
#' Divide a collection into groups determined by a predicate.
#'
#' @param
#'      pred a binary predicate. The predicate to test whether
#'      a split should be introduced between the two elements.
#'
#' @param
#'      coll a collection. The collection to split.
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
#'    inst/examples/example-xSplitWith.R
#'
#' @rdname xSplitWith
#' @export

xSplitWith <- local({

	bisect <- MakeFun(function (pred, coll, invoking_call) {
		# split a collection into a head and tail
		# using a predicate.

		for (ith in 1:(length(coll) - 1)) {

			is_match <- pred( coll[[ith]], coll[[ith + 1]] )

			MACRO( Must $ Be_Flag(is_match, pred) )

			if (isTRUE(is_match)) {

				return (
					list(
						head(coll, ith),
						tail(coll, -ith)) )
			}
		}

		list(coll, list())
	})

	MakeFun(function (pred, coll) {

			if (length(coll) == 0) {
			list()
		} else if (length(coll) == 1) {
			list(as.list(coll))
		} else {

			cleaved <- list()
			cleaved_current <- 1

			while (length(coll) > 0) {

				trimmed <- bisect(pred, coll, sys.call())
				cleaved[cleaved_current] <- list(as.list(trimmed[1]))

				coll <- trimmed[[2]]
				cleaved_current <- cleaved_current + 1
			}

			cleaved
		}
	})

})

#' @rdname xSplitWith
#' @export

xSplitWith_ <- MakeVariadic(xSplitWith, 'coll')
