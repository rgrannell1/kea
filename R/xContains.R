
#' xContains
#'
#' Check if a collection contains a value.
#'
#' @param coll a collection.
#' @param val an arbitrary value.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     various types of NA are not-distinguished between.
#'     Type conversion is not carried out.
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xContains <- function (coll, val) {
	# Collection any -> any -> Vector logical

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))
	assert(
		!missing(val), parent_call,
		exclaim$parameter_missing(val))

	coll <- dearrowise(coll)
	val <- dearrowise(val)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		logical(0)
	} else {
		is_match <- vapply(
			coll,
			function (elem) {
				identical(elem, val, single.NA = True)
			},
			logical(1),
			USE.NAMES = False)

		if ( all(is.na(is_match)) ) {
			False
		} else {
			any(is_match)
		}
	}
}

#' @export

xContains... <- function (..., val) {
	xContains(list(...), val)
}