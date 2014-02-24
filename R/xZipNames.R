
#' xZipNames
#'
#' Convert a list of name, value pairs into a named list.
#'
#'
#' @param
#'    colls a list or pairlist of two-element lists or pairlists,
#'    with the first element being a string and the second
#'    element being an arbitrary value. The string part of each
#'    sublist is used as the name in the output list, and the
#'    remaining part is used as the value associated with that key.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @family name_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xZipNames.R
#'
#' @rdname xZipNames
#' @export

xZipNames <- function (colls) {
	# Collection Collection any -> [any]
	# take a collection of name:value pairs and associate
	# them into a named list.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(colls)
	insist $ must_be_collection(colls, invoking_call)

    insist $ must_be_collection_of_collections(colls, invoking_call)
    insist $ must_be_collection_of_lengths(colls, 2, invoking_call)

	if (length(colls) == 0) {
		list()
	} else {

		keys <- vapply(
			colls,
			function (coll) {

				key <- as_typed_vector(coll[[1]], "character")

				insist $ must_be_of_length(key, 1, invoking_call)

				key
			},
			character(1), USE.NAMES = False)

		structure(
			Map( function (elem) elem[[2]], colls ),
			names = keys)
	}
}

#' @rdname xZipNames
#' @export

xZipNames... <- function (...) {
	xZipNames(list(...))
}
