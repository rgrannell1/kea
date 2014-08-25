
#' xJoin
#'
#' Concatenate several collections into one collection.
#'
#' @section Type Signature:
#'    ||any|| -> [any]
#'
#' @details
#'     xJoin is useful for flatting a collection by one
#'    level; it can take a list of lists, and return
#'    a list of each item joined end to end.
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
#'    the second, without modification.
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

xJoin <- MakeFun('xJoin', function (colls) {

	if (length(colls) == 0) {
		list()
	} else {
		colls <- lapply(colls, as.list)
		as.list(do.call(c, colls))
	}
})

#' @rdname xJoin
#' @export

xJoin_ <- MakeVariadic(xJoin, 'colls')
