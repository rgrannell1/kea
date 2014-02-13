
#' xPluck
#'
#' Map over a collection of lists or pairlists,
#' selecting fields in each element by name.
#'
#' @param
#'    str a string. The key to select from each collection.
#'
#' @param
#'    colls a collections of collections. The collection
#'    of collections to select keys from.
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
#'
#' @example
#'    inst/examples/example-xPluck.R
#'
#' @rdname xPluck
#' @export

xPluck <- function (str, colls) {
	# Vector string -> Collection any -> Collection [any]

	invoking_call <- sys.call()

	insist $ must_not_be_missing(str)
	insist $ must_not_be_missing(colls)

	insist $ must_be_collection(str, invoking_call)
	insist $ must_be_collection(colls, invoking_call)

	str <- unit_to_value(as_atom(str, "character"))

	insist $ must_be_collection_of_collections(colls, invoking_call)

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
