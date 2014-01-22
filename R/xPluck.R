
#' xPluck
#'
#' Map over a collection of lists or pairlists,
#' selecting fields in each element by name.
#'
#' @param
#'    str a string.
#'
#' @param
#'    colls a list or pairlist of lists or pairlists.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{colls} is length-zero.
#'    If str is length-zero then the empty string "" is
#'    used to match key-names.
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

xPluck <- function (str, colls) {
	# Vector string -> Collection any -> Collection [any]

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	str <- as_typed_vector(str, "character", True)

	insist$must_be_of_length(str, 1, invoking_call)
    insist$must_be_collection_of_collections(colls, invoking_call)

	if (length(colls) == 0) {
		list()
	} else {
		lapply( colls, function (elem) {
			as.list( elem[[str]] )
		})
	}
}

#' @rdname xPluck
#' @export

xPluck... <- function (str, ...) {
	xPluck(str, list(...))
}
