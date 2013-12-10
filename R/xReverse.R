
#' xReverse
#'
#' Reverse a collection.
#'
#' @param
#'    fn an function of any arity.
#'
#' @return
#'    a positive whole number.
#'
#' @section Corner Cases:
#'    reversing the empty list yields the empty list.
#'
#' @family
#'    collection_functions
#'
#' @export

xReverse <- function (coll) {
	# reverse a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		list()
	} else {
		as.list(rev(coll))
	}
}

xReverse... <- function (...) {
	xReverse(list(...))
}
