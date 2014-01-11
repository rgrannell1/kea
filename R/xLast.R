
#' xLast
#'
#' Return the last element in a collection.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The value of the last element in \code{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \code{coll} has less than one element; this is
#'    because any other corner case would violate the function's type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xLast
#' @export

xLast <- function (coll) {
	# Collection any -> any
	# return the last element of a collection x,
	# using the subset operator

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	assert(
		length(coll) > 0, invoking_call,
		exclaim$must_be_lequal_than(
			coll, 0, summate(coll)) )

	coll[[ length(coll) ]]
}

#' @rdname xLast
#' @export

xLast... <- function (...) {
	xLast(list(...))
}
