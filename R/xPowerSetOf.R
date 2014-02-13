
#' xPowerSetOf
#'
#' Enumerate every way of subsetting a collection.
#'
#' @details
#'     \bold{xPowerSetOf} generates the set of all subsets of a collection.
#'     This set has length \bold{2^length(coll)}.

#' @param
#'     coll a collection. The collection to return the subsets of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'     A list of lists.
#'
#' @family combinatoric_functions
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPowerSetOf.R
#'
#' @rdname xPowerSetOf
#' @export

xPowerSetOf <- function (coll) {
	# Collection any -> [any]
	# get the power set of a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)
	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		subsets <- list(list())
		coll <- lapply(coll, list)

		for (elem in coll) {
			elem_subsets <- vector('list', length(subsets))

			# append the element to each set in the partial
			# powerset, until all elements are added. Tree-like branching.

			for (ith in seq_along(subsets)) {
				elem_subsets[[ith]] <- c(subsets[[ith]], elem)
			}
			subsets <- c(subsets, elem_subsets)
		}

		subsets
	}
}

#' @rdname xPowerSetOf
#' @export

xPowerSetOf... <- function (...) {
	xPowerSetOf(list(...))
}
