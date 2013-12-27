
#' xSegment
#'
#' Divide a collection into segments of fixed length.
#'
#' @section Uses:
#'    \code{xSegment} is useful for reshaping a collection into pairs, triples,
#'    or larger groups before applying a function to each group.
#'
#' @param
#'    num a nonnegative whole number.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a list of n-element lists.
#'
#' @section Corner Cases:
#'    the final list in the return value will have less than \code{num}
#'    elements if \code{length(coll)} is not evenly divisible by \code{num}.
#'    if \code{coll} is length-zero, the empty list is returned.
#'
#' @family collection_functions
#'
#' @export

xSegment <- function (num, coll) {
	# integer -> Collection any -> [[any]]
	# groups coll into chunks of num,
	# when possible.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) %in% 0:1, invoking_call,
		exclaim$must_have_length(
			num, 0:1, profile_object(num)) )

	assert(
		num > 0, invoking_call,
		exclaim$must_be_greater_than(
			num, 0, profile_object(num)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	if (length(coll) == 0) {
		list()
	} else if (is.infinite(num)) {
		list(as.list(coll))
	} else {
		lapply(
			seq(1, to = length(coll), by = num),
			function (lower) {
				as.list(coll[ lower:min(length(coll), lower + num - 1) ])
		})
	}
}

#' @export

xSegment... <- function (num, ...) {
	xSegment(num, list(...))
}
