
#' xIsEmpty
#'
#' Is a collection length-zero?
#'
#' @param coll a collection.
#'
#' @return a boolean value.
#'
#'
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xIsEmpty <- function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	length(coll) == 0
}

#' @export

xIsEmpty... <- function (...) {
	xIsEmpty(list(...))
}
