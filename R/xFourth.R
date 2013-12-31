
#' xFourth
#'
#' Return the fourth value in a collection.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    the fourth value in \code{coll}.
#'
#' @section Corner Cases:
#'    throws an error if \code{coll} has less than four element; this is
#'    because any other corner case would violate the functions type-signature.
#'
#' @family collection_functions
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xFourth
#' @export

xFourth <- function (coll) {
	# Collection any -> any
	# return the fourth element of a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	assert(
		length(coll) >= 4, invoking_call,
		exclaim$must_be_longer_than(
			coll, 3, profile_object(coll)) )

	coll[[4]]
}

#' @rdname xFourth
#' @export

xFourth... <- function (...) {
	xFourth(list(...))
}
