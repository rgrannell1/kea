
#' xInter
#'
#' Get the set intersection of several collections.
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
	# Collection any -> Collection any -> Collection any
	# get the set intersection of two collections.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(colls) )
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

xInter... <- function (...) {
	xInter(list(...))
}
