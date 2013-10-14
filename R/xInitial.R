
#' xInit
#' 
#' Remove the first element from a collection.
#'
#' @param coll a collection.
#'
#' @return a list.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xInit <- function (coll) {
	# Collection any -> [any]
	# return everything but the first element of a 
	# collection.

	pcall <- sys.call()

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || length(coll) == 1) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
}
