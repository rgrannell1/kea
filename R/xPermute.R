
#' xPermute
#'
#' Permute several collections using indices.
#'
#' @param coll a collection of whole numbers.
#' @param colls several collections of equal length to \code{coll}.
#'
#' @return a list of collections.
#'
#' @section Corner Cases:
#'     returns the empty list is \code{coll} is length-zero.
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R

#' @export

xPermute <- function (coll, colls) {
#' Permute several collections using indices.

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	assert(
		!missing(colls), parent_call,
		exclaim$parameter_missing(colls))

	coll <- dearrowise(coll)
	colls <- dearrowise(colls)

	coll <- as_typed_vector(coll, 'numeric')

	assert(
		all(sapply(colls, length) == length(coll)), parent_call,
		exclaim$must_be_collection_of_length(colls, length(coll)) )

	if (length(coll) == 0) {
		list()
	} else {
		lapply(colls, function (permutable) {
			permutable[coll]
		})
	}
}

#' @export

xPermute... <- function (coll, ...) {
	xPermute(coll, list(...))
}
