
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
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'    If str is length-zero then the empty string "" is
#'    used to match key-names.
#'
#' @family higher_order_functions
#'
#' @family collection_functions
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
			str, 1, profile_object(str)) )

	assert(
		is_recursive(coll), invoking_call,
		exclaim$must_be_recursive(
			coll, profile_object(coll)) )

	assert(
		all(sapply(coll, is_recursive)), invoking_call,
		exclaim$must_be_recursive_of_collections(
			coll, profile_object(coll)) )

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
