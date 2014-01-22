
#' xDissoc
#'
#' Split a named list into a list of name: value list pairs.
#'
#' @param
#'    colls a list or pairlist of list or pairlist pairs, with the first element being a
#'	  string and the second element being any value.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'      Returns \code{list()} if \code{colls} is length-zero.
#'
#' @family reshaping_functions
#'
#' @family name_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xDissoc
#' @export

xDissoc <- function (colls) {
	# Named collsection any -> [[string, any]]
	# split a list into its names and values.

	invoking_call <- sys.call()

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	insist$must_be_fully_named(colls, invoking_call)
    insist$must_be_collection_of_collections(colls, invoking_call)

	if (length(colls) == 0) {
		list()
	} else {

		colnames <- names(colls)
		colls <- unname(colls)

		lapply(seq_along(colls), function (ith) {

			list(
				colnames[[ith]],
				colls[[ith]] )
		})
	}
}

#' @rdname xDissoc
#' @export

xDissoc... <- function (...) {
	xDissoc(list(...))
}
