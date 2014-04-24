
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
#' @family key_functions
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

xPluck <- MakeFun(function (str, colls) {
	# Vector string -> Collection any -> Collection [any]

	MACRO( Must $ Not_Be_Missing(str) )
	MACRO( Must $ Not_Be_Missing(colls) )

	MACRO( Must $ Be_Collection(str) )
	MACRO( Must $ Be_Collection(colls) )

	str <- unit_to_value(as_atom(str, "character"))

	MACRO( Must $ Be_Collection_Of_Collections(colls) )

	if (length(colls) == 0) {
		list()
	} else {
		lapply( colls, function (elem) {
			elem[[str]]
		})
	}
})

#' @rdname xPluck
#' @export

xPluck_ <- function (str, ...) {
	xPluck(str, list(...))
}
