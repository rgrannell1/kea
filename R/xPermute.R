
#' xPermute
#'
#' Permute several collections using indices.
#'
#' @details
#'    \bold{xPermute} allows several collections to be rearranged
#'    in the same way simultaneously.
#'
#'    \code{colls <- list(ids = list(15, 1, 12), ages = list(17, 29, 24), weights = list(80, 82, 76))}
#'
#'    \code{xPermute(xOrder( xFirstOf(colls) ), colls)}
#'
#' @param
#'    nums a collection of whole numbers. The indices
#'    by which to permute the input collections.
#'
#' @param
#'    colls several collections of equal length to \bold{nums}. The
#'    collections to permute.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of collections.
#'
#' @section Corner Cases:
#'    returns the empty list is \bold{nums} is length-zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPermute.R
#'
#' @rdname xPermute
#' @export

xPermute <- function (nums, colls) {
	# Vector integer -> [[any]] -> [[any]]
	# Permute several collections using indices.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	nums <- as_typed_vector(nums, 'numeric')

	insist $ must_be_collection(colls, invoking_call)
	insist $ must_be_collection_of_collections(colls, invoking_call)
	insist $ must_be_collections_of_length_matching(colls, nums, invoking_call)

	if (length(nums) == 0) {
		list()
	} else {
		lapply(colls, function (permutable) {
			permutable[nums]
		})
	}
}

#' @rdname xPermute
#' @export

xPermute... <- function (nums, ...) {
	xPermute(nums, list(...))
}
