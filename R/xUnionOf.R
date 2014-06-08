
#' xUnionOf
#'``
#' Get the set union of several collections.
#'
#' @section Type Signature:
#'     ||any|| -> [any]
#'
#' @details
#'    \code{xUnionOf} returns a list of the unique
#'    elements in the combination of each collection
#'    in \code{colls}.
#'
#' @param
#'    colls a collection of collections. The collections
#'    to take the union of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{colls} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xUnionOf.R
#'
#' @rdname xUnionOf
#' @export

xUnionOf <- MakeFun(function (colls) {

	MACRO( Fix(xUnionOf, colls) )

	MACRO( Must $ Be_Collection(colls) )
	MACRO( Must $ Be_Collection_Of_Collections(colls) )

	if (length(colls) == 0) {
		list()
	} else {

		# -- ensure that a list is returned.
		colls <- lapply(colls, as.list)

		unique(do.call(c, colls))
	}
})

#' @rdname xUnionOf
#' @export

xUnionOf_ <- MakeVariadic(xUnionOf, 'colls')
