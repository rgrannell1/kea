
#' xIsMember
#'
#' Check if a collection contains a value.
#'
#' @param
#'    val an arbitrary value.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    various types of \code{Na} are not-distinguished between.
#'    Type conversion is not carried out.
#'
#' @family collection_functions
#'
#' @export

xIsMember <- function (val, coll) {
	# Collection any -> any -> Vector logical
	# check if a collection contains a value.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))
	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		logical(0)
	} else {
		is_match <- vapply(
			coll,
			function (elem) {
				identical(elem, val, single.NA = True)
			},
			logical(1), USE.NAMES = False)

		if ( all(is.na(is_match)) ) {
			False
		} else {
			any(is_match)
		}
	}
}

#' @export

xIsMember... <- function (..., val) {
	xIsMember(list(...), val)
}
