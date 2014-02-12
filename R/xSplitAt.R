
#' xSplitAt
#'
#' Split a collection into elements before and upto
#' an index, and after that index.
#'
#' @param
#'    nums a vector of nonnegative whole numbers.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of two lists; the first list containing
#'    the first \code{nums} elements of \bold{coll}, and the
#'    second list containing the remaining elements \bold{coll}.
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

xSplitAt <- function (nums, coll) {
	# numsber -> Collection any -> [[any], [any]]
	# take the first n values of collection.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(nums, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	nums <- unit_to_value(as_typed_vector(nums, "numeric"))

	insist $ must_be_collection(coll, invoking_call)
	insist $ must_be_collection(nums, invoking_call)

	nums <- unit_to_value(as_typed_vector(nums, 'numeric'))

	insist $ must_be_whole(nums, invoking_call)
	insist $ must_be_nonnegative(nums, invoking_call)
	insist $ max_must_be_less_than_length_of(nums, coll, invoking_call)

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
}

#' @rdname xSplitAt
#' @export

xSplitAt... <- function (nums, ...) {
	xSplitAt(nums, list(...))
}
