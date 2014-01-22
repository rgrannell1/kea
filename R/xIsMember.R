
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
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    Various types of \code{Na} are not-distinguished between.
#'    Type conversion is not carried out.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xIsMember
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

	insist$must_be_collection(coll, invoking_call)

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

#' @rdname xIsMember
#' @export

xIsMember... <- function (val, ...) {
	xIsMember(val, list(...))
}
