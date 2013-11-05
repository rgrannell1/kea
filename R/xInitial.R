
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

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || length(coll) == 1) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
}
