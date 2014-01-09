
#' xPluck
#'
#' Map over a collection of lists or pairlists,
#' selecting fields in each element by name.
#'
#' @param
#'    str a string.
#'
#' @param
#'    coll a list or pairlist of lists or pairlists.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'    If str is length-zero then the empty string "" is
#'    used to match key-names.
#'
#'
#' @family selection_functions
#'
#' @family name_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xPluck
#' @export

xPluck <- function (str, coll) {
	# Vector string -> Collection any -> Collection [any]

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))
	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	str <- as_typed_vector(str, "character", True)

	assert(
		length(str) == 1, invoking_call,
		exclaim$must_have_length(
			str, 1, summate(str)) )

	assert(
		is_recursive(coll), invoking_call,
		exclaim$must_be_recursive(
			coll, summate(coll)) )

	assert(
		all( vapply(coll, is_recursive, logical(1)) ), invoking_call,
		exclaim$must_be_recursive_of_collections(
			coll, summate(coll)) )

	if (length(coll) == 0) {
		list()
	} else {
		lapply( coll, function (elem) {
			as.list( elem[[str]] )
		})
	}
}

#' @rdname xPluck
#' @export

xPluck... <- function (str, ...) {
	xPluck(str, list(...))
}
