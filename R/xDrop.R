
#' xDrop
#'
#' Take several elements from the front of a collection.
#'
#' @param
#'    num a nonnegative whole number.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    if \code{coll} is empty the empty list is returned.
#'
#' @family
#'    collection_functions
#'
#' @export

xDrop <- function (num, coll) {
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
		exclaim$must_be_numeric(num))

	assert(
		round(num) == num, invoking_call,
		exclaim$must_be_whole(num))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || num >= length(coll)) {
	 	list()
	} else {
		as.list(coll)[(num + 1) : length(coll)]
	}
}

#' @export

xDrop... <- function (num, ...) {
	xDrop(num, list(...))
}
