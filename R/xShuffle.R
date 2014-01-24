
#' xShuffle
#'
#' Randomly rearrange a collection.
#'
#' @param
#'      coll a collection. The collection to rearrange.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A list.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xShuffle
#' @export

xShuffle <- function (coll) {
	# Collection any -> [any]
	# shuffle a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1)
		as.list(coll)
	else {
		as.list(sample(coll))
	}
}

#' @rdname xShuffle
#' @export

xShuffle... <- function (...) {
	xShuffle(list(...))
}