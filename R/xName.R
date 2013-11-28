
#' xName
#'
#' Add names to a collection.
#'
#' @param strs a collection of strings.
#' @param coll a collection
#'
#' @return a named list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xName <- function (strs, coll) {
	# Vector string -> Collection any -> [any]
	# add names to a collection.

	parent_call <- sys.call()

	assert(
		!missing(strs), parent_call,
		exclaim$parameter_missing(strs))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	assert(
		is_collection(strs), parent_call,
		exclaim$must_be_collection(strs))

	strs <- as_typed_vector(strs, 'character')

	assert(
		length(strs) == length(coll), parent_call,
		exclaim$must_have_equal_lengths(strs, colls))

	structure(as.list(coll), names = strs)
}
