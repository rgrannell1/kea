
#' xAssoc
#'
#' Convert a list of name, value pairs into a named list.
#'
#' @param
#'    colls a list or pairlist of list or pairlist pairs,
#'    with the first element being a
#'	  string and the second element being any value.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @family name_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAssoc.R
#'
#' @rdname xAssoc
#' @export

xAssoc <- function (colls) {
	# Collection Collection any -> [any]
	# take a collection of name:value pairs and associate
	# them into a named list.

	invoking_call <- sys.call()

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	assert(
		is_recursive(colls), invoking_call,
		exclaim$must_be_recursive(
			colls, summate(colls)) )

	assert(
		all( vapply(colls, length, integer(1)) == 2), invoking_call,
		exclaim$must_be_collection_of_length(
			colls, 2, summate(colls)) )

	if (length(colls) == 0) {
		list()
	} else {
		keys <- vapply(
			colls,
			function (coll) {

				key <- coll[[1]]
				key <- as_typed_vector(key, "character")

				assert(
					length(key) == 1, invoking_call,
					exclaim$must_be_lequal_than(key, 1))

				key
			},
			character(1), USE.NAMES = False)

		structure(
			Map( function (elem) elem[[2]], colls ),
			names = keys)
	}
}

#' @rdname xAssoc
#' @export

xAssoc... <- function (...) {
	xAssoc(list(...))
}
