
#' xTake
#'
#' Take several elements from the head of a collection.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	 If \code{coll} is empty the empty list is returned.

#' @family collection_functions
#'

#'
#' @export

xTake <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) == 1, invoking_call,
		exclaim$must_have_length(num, 1))

	assert(
		num >= 0, invoking_call,
		exclaim$must_be_greater_than(num, 0))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || num == 0) {
		list()
	} else {
		as.list(coll)[seq_len( min(num, length(coll)) )]
	}
}

#' @export

xTake... <- function (num, ...) {
	xTake(num, list(...))
}
