
#' xRest
#'
#' Return a list excluding the first element of a collection.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xRest
#' @export

xRest <- function (coll) {
	# Collection a -> [a]
	# return everything but the first element of a
	# collection x.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)))

	if (length(coll) < 2) {
		list()
	} else {
		as.list( coll[-1] )
	}
}

#' @rdname xRest
#' @export

xRest... <- function (...) {
	xRest(list(...))
}
