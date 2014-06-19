
#' xInterOf
#'
#' Get the set intersection of several collections.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    colls a collection of collections. The collections to
#'    take the intersection of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xInterOf.R
#'
#' @rdname xInterOf
#' @export

xInterOf <- MakeFun(function (colls) {

	if (length(colls) == 0) {
		list()
	} else if (length(colls) == 1) {
		colls[[1]]
	} else {

		overlap <- colls[[1]]

		for (ith in 2:length(colls)) {
			overlap <- intersect( overlap, colls[[ith]] )
		}

		as.list(overlap)
	}
})

#' @rdname xInterOf
#' @export

xInterOf_ <- MakeVariadic(xInterOf, 'colls')
