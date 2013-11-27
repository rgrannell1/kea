
#' xPluck
#'
#' Map over a collection of lists or pairlists,
#'     selecting fields in each element by name.
#'
#' @param str a string.
#' @param coll a list or pairlist of lists or pairlists.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero. If str is length-zero
#'     then the empty string "" is used to match key-names.
#'
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xPluck <- function (str, coll) {
	# Vector string -> Collection any -> Collection [any]

	parent_call <- sys.call()

	assert(
		!missing(str), parent_call,
		exclaim$parameter_missing(str))
	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	str <- dearrowise(str)
	coll <- dearrowise(coll)

	str <- as_typed_vector(str, "character", True)

	assert(
		length(str) == 1, parent_call,
		exclaim$must_have_length(str, 1))

	assert(
		is_recursive(coll), parent_call,
		exclaim$must_be_recursive(coll))

	assert(
		all(sapply(coll, is_recursive)), parent_call,
		exclaim$must_be_recursive_of_collections(coll))

	if (length(coll) == 0) {
		list()
	} else {
		lapply( coll, function (elem) {
			as.list( elem[[str]] )
		})
	}
}

#' @export

xPluck... <- function (str, ...) {
	xPluck(str, list(...))
}
