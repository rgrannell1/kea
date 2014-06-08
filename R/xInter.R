
#' xInter
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
#'    inst/examples/example-xInter.R
#'
#' @rdname xInter
#' @export

xInter <- MakeFun(function (colls) {

	MACRO( Fix(xInter, colls) )

	MACRO( Must $ Be_Collection(colls) )

	MACRO( Must $ Be_Collection_Of_Collections(colls) )

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

#' @rdname xInter
#' @export

xInter_ <- MakeVariadic(xInter, 'colls')
