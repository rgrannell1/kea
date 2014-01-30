
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
#' @example
#'    inst/examples/example-xZip.R
#'
#' @rdname xZip
#' @export

xZip <- function (colls) {
	#
	# zip collections together

	invoking_call <- sys.call()

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	if (length(colls) == 0 || length(colls)[[1]] == 0) {
		list()
	} else {

		insist $ must_be_collection(colls, invoking_call)
		insist $ must_be_collection_of_collections(colls, invoking_call)
		insist $ must_be_collection_of_equal_length(colls, invoking_call)

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
