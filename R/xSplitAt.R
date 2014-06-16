
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

	# nums <- unit_to_value(as_typed_vector(nums, 'numeric'))

	MACRO( Must $ All_Be_Whole(nums) )
	MACRO( Must $ Be_Positive_Indices(nums, coll) )

	if (length(coll) == 0) {
		list()
	} else {

		lapply(
			xSplitWith(
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

xSplitAt_ <- MakeVariadic(xSplitAt, 'coll')
