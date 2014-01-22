
#' xFirstOf
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
#'    The first element in \code{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \code{coll} has less than one element; this is
#'    because any other corner case would violate the functions type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xFirstOf
#' @export

xFirstOf <- function (coll) {
	# Collection any -> any
	# return the first element of a collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)
	insist$must_be_longer_than(coll, 1, invoking_call)

	coll[[1]]
}

#' @rdname xFirstOf
#' @export

xFirstOf... <- function (...) {
	xFirstOf(list(...))
}
