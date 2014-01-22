
#' xPack
#'
#' Remove all length-zero values from a collection.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    Returns a list of elements in \code{coll},
#'	  with all length-zero values removed.
#'
#' @section Corner Cases:
#'    Returns the emty list if \code{coll} is length-zero,
#'    or all elements in \code{coll} are length-zero.
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xPack
#' @export

xPack <- function (coll) {
	# Collection any -> [any]
	# remove all length-zero elements from a coll

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		coll[vapply(coll, length, integer(1)) != 0]
	}
}

#' @rdname xPack
#' @export

xPack... <- function (...) {
	xPack(list(...))
}
