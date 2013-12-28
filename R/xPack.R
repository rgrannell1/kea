
#' xPack
#'
#' Remove all length-zero values from a collection.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    returns a list of elements in \code{coll},
#'	  with all length-zero values removed.
#'
#' @section Corner Cases:
#'    Returns the emty list if \code{coll} is length-zero,
#'    or all elements in \code{coll} are length-zero.
#'
#' @family collection_functions
#'
#' @family filtering_functions
#'
#' @export

xPack <- function (coll) {
	# Collection any -> [any]
	# remove all length-zero elements from a coll

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	if (length(coll) == 0) {
		list()
	} else {
		coll[sapply(coll, length) != 0]
	}
}

#' @export

xPack... <- function (...) {
	xPack(list(...))
}
