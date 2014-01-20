
#' xZip
#'
#' Generate a list of n-element lists from n collections.
#'
#' @param
#'    colls n-vectors, lists or pairlists.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    Returns a list.
#'
#' @section Corner Cases:
#'    The empty list is returned if the shortest collection
#'    has length-zero, or no collections are included.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xZip
#' @export

xZip <- function (colls) {

	invoking_call <- sys.call()

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	assert(
		all(sapply(colls, is_collection)), invoking_call,
		exclaim$must_be_recursive_of_collections(
			colls, summate(colls))
	)

	assert(
		length(unique( vapply(colls, length, integer(1)) )) == 1,
		invoking_call,
		exclaim$must_be_collection_of_equal_lengths(
			colls, summate(colls)) )

	if (length(colls) == 0 || length(colls)[[1]] == 0) {
		list()
	} else {

		lapply(
			seq_along( colls[[1]] ),
			function (ith_elem) {

				lapply( colls, function (coll) {
					coll[[ith_elem]]
				})
		})
	}
}

#' @rdname xZip
#' @export

xZip... <- function (...) {
	xZip(list(...))
}
