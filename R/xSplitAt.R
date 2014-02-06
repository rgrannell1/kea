
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

	insist $ must_be_whole(nums, invoking_call)
	insist $ must_be_nonnegative(nums, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {

		coll <- as.list(coll)

		nums[nums > length(coll)] <- length(coll)
		nums <- sort( unique(c(nums, length(coll))) )

		lower <- 1
		for ( ith in seq_len(length(nums)) ) {

			upper <- nums[[ith]]
			bounds <- c(bounds, list( coll[lower:upper] ))
			lower <- upper + 1
		}

		bounds
	}
}

#' @rdname xSplitAt
#' @export

xSplitAt... <- function (nums, ...) {
	xSplitAt(nums, list(...))
}
