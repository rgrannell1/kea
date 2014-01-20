
#' xPermute
#'
#' Permute several collections using indices.
#'
#' @param
#'    coll a collection of whole numbers.
#'
#' @param
#'    colls several collections of equal length to \code{coll}.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of collections.
#'
#' @section Corner Cases:
#'    returns the empty list is \code{coll} is length-zero.
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

	assert(
		all(vapply(colls, length, integer(1)) == length(coll)),
		invoking_call,
		exclaim$must_be_collection_of_length(
			colls, length(coll), summate(colls)) )

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
