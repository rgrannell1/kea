
#' xSplitAt
#'
#' Split a collection into at several indices.
#'
#' @param
#'    nums a vector of nonnegative whole numbers. The indices
#'    to split a collection \bold{after}.
#'
#' @param
#'    coll a collection. The collection to split.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of lists.
#'
#' @section Corner Cases:
#'    If \code{nums} is zero then the first list in the
#'    returned value is empty.
#'    Likewise, if \code{nums} is equal or larger than the
#'    length of \bold{coll} then
#'    the second return list is empty. If \bold{coll} is
#'    length zero both lists are empty.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSplitAt.R
#'
#' @rdname xSplitAt
#' @export

xSplitAt <- MakeFun(function (nums, coll) {
	# numsber -> Collection any -> [[any], [any]]
	# take the first n values of collection.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(nums) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(nums) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

	nums <- unit_to_value(as_typed_vector(nums, 'numeric'))

	MACRO( arrow ::: Must $ Be_Whole(nums) )
	insist $ must_be_positive_indices_of(nums, coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {

		lapply(
			xSplitBy(
				function (ith, drop) {
					ith %in% nums
				},
				seq_along(coll)
			),
			function (indices) {
				coll[unlist(indices)]
			}
		)

	}
})

#' @rdname xSplitAt
#' @export

xSplitAt... <- function (nums, ...) {
	xSplitAt(nums, list(...))
}
