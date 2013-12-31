
#' xFirst
#'
#' Return the first element of a collection.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    the first element in \code{coll}.
#'
#' @section Corner Cases:
#'    throws an error if \code{coll} has less than one element; this is
#'    because any other corner case would violate the functions type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xFirst
#' @export

xFirst <- function (coll) {
	# Collection any -> any
	# return the first element of a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	assert(
		length(coll) >= 1, invoking_call,
		exclaim$must_be_longer_than(coll, 1))

	coll[[1]]
}

#' @rdname xFirst
#' @export

xFirst... <- function (...) {
	xFirst(list(...))
}
