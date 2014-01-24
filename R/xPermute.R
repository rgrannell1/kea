
#' xPermute
#'
#' Permute several collections using indices.
#'
#' @param
#'    coll a collection of whole numbers.
#'
#' @param
#'    colls several collections of equal length to \bold{coll}.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of collections.
#'
#' @section Corner Cases:
#'    returns the empty list is \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xPermute
#' @export

xPermute <- function (coll, colls) {
	# Vector integer -> [[any]] -> [[any]]
	# Permute several collections using indices.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	coll <- as_typed_vector(coll, 'numeric')

	insist$must_be_collection(colls, invoking_call)
	insist$must_be_collection_of_collections(colls, invoking_call)
	insist$must_be_collections_of_length_matching(colls, coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		lapply(colls, function (permutable) {
			permutable[coll]
		})
	}
}

#' @rdname xPermute
#' @export

xPermute... <- function (coll, ...) {
	xPermute(coll, list(...))
}
