
#' xDissoc
#'
#' Split a named list into a list of name: value lists.
#'
#' @param coll a list or pairlist of list or pairlist pairs, with the first element being a
#'	string and the second element being any value.
#'
#' @return a named list.
#'
#' @section Corner Cases:
#'     returns \code{list()} if \code{coll} is length-zero.
#' @template glossary
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xDissoc <- function (coll) {
	# Named Collection any -> [[string, any]]
	# split a list into its names and values.

	parent_call <- sys.call()

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		length(names(coll)) == length(coll), parent_call,
		exclaim$must_be_named(coll))

	if (length(coll) == 0) {
		list()
	} else {

		lapply(seq_along(coll), function (ith) {

			list(
				names( coll )[[ith]],
				unname( coll[[ith]] ))
		})
	}
}

#' @export

xDissoc... <- function (...) {
	xDissoc(list(...))
}
