
#' xDuplicatesOf
#'
#' Return the duplicated elements in a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    coll a collection. The collection to return
#'    duplicated elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{colls} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDuplicatesOf.R
#'
#' @rdname xDuplicatesOf
#' @export

xDuplicatesOf <- MakeFun(function (coll)

	if (length(coll) == 0)
		list()
	else
		as.list(coll[ duplicated(coll) ])

)

#' @rdname xDuplicatesOf
#' @export

xDuplicatesOf_ <- MakeVariadic(xDuplicatesOf, 'coll')
