
#' xJoin
#'
#' Concatenate several collections into one collection.
#'
#' @param
#'    colls a collection of collections. The collections
#'    to concatenate to each other.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Empty collections act as a unit for concatenation;
#'    concatenating the empty list to another list returns
#'    the second list, without modification.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xJoin.R
#'
#' @family reshaping_functions
#'
#' @rdname xJoin
#' @export

xJoin <- MakeFun(function (colls) {
	# Collection any coll -> [any]
	# Concatenate several collections
	# into one list.

	MACRO( Must $ Not_Be_Missing(colls) )
	MACRO( Must $ Be_Collection(colls) )

	MACRO( Must $ Be_Collection_Of_Collections(colls) )

	if (length(colls) == 0) {
		list()
	} else {
		colls <- lapply(colls, as.list)
		as.list(do.call(c, colls))
	}
})

#' @rdname xJoin
#' @export

xJoin... <- function (...) {
	xJoin(list(...))
}
