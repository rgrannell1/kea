
#' xUnite
#'``
#' Get the set union of two collections.
#'
#' @section Type Signature:
#'     ||any|| -> [any]
#'
#' @details
#'    \code{xUnite} returns a list of the unique
#'    elements in both \bold{coll1} and \bold{coll2}.
#'
#' @param
#'    coll1 a collection. The first collection to get elements from.
#'
#' @param
#'    coll2 a collection. The second collection to get elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll1} or \bold{coll2} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xUnite.R
#'
#' @rdname xUnite
#' @export

xUnite <- MakeFun('xUnite', function (coll1, coll2) {

	if (length(coll1) == 0 || length(coll2) == 0) {
		list()
	} else {
		unique(c(list(), coll1, coll2))
	}
})

#' @rdname xUnite
#' @export

xUnite_ <- MakeVariadic(xUnite, 'coll2')
