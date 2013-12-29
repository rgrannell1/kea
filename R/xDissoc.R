
#' xDissoc
#'
#' Split a named list into a list of name: value list pairs.
#'
#' @param
#'    coll a list or pairlist of list or pairlist pairs, with the first element being a
#'	  string and the second element being any value.
#'
#' @return
#'    a named list.
#'
#' @section Corner Cases:
#'     returns \code{list()} if \code{coll} is length-zero.
#'
#' @family collection_functions
#'
#' @family reshaping_functions
#'
#' @family name_functions
#'
#' @family variadic_functions
#'
#' @rdname xDissoc
#' @export

xDissoc <- function (coll) {
	# Named Collection any -> [[string, any]]
	# split a list into its names and values.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		length(names(coll)) == length(coll), invoking_call,
		exclaim$must_be_named(
			coll, profile_object(coll)) )

	if (length(coll) == 0) {
		list()
	} else {

		colnames <- names(coll)
		coll <- unname(coll)

		lapply(seq_along(coll), function (ith) {

			list(
				colnames[[ith]],
				coll[[ith]] )
		})
	}
}

#' @rdname xDissoc
#' @export

xDissoc... <- function (...) {
	xDissoc(list(...))
}
