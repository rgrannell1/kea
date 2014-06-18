
#' xPowerSetOf
#'
#' Enumerate every way of subsetting a collection.
#'
#' @section Type Signature:
#'     |any| -> [[any]]
#'
#' @details
#'     \bold{xPowerSetOf} generates the set of all subsets of a collection.
#'     This set has length \bold{2^length(coll)}, so inputs longer than
#'     twenty elements will take a very long time to compute.
#'
#' @param
#'     coll a collection. The collection to return the subsets of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'     A list of lists.
#'
#' @section Corner Cases:
#'     Returns the empty list if \bold{coll} is length-zero.
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

xPowerSetOf <- MakeFun(function (coll) {

	if (length(coll) == 0) {
		list()
	} else {
		subsets <- list(list())
		coll <- lapply(coll, list)

		for (elem in coll) {
			elem_subsets <- vector('list', length(subsets))

			# -- append the element to each set in the partial
			# -- powerset, until all elements are added. Tree-like branching.

			for (ith in seq_along(subsets)) {
				elem_subsets[[ith]] <- c(subsets[[ith]], elem)
			}
			subsets <- c(subsets, elem_subsets)
		}

		subsets
	}
})

#' @rdname xPowerSetOf
#' @export

xPowerSetOf_ <- MakeVariadic(xPowerSetOf, 'coll')
