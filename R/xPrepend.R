
#' xPrepend
#'
#' Add a value to the front of a collection.
#'
#' @section Type Signature:
#'    any -> |any| -> [any]
#'
#' @param
#'    val an arbitrary value. The value to prepend.
#'
#' @param
#'     coll a collection. The collection to add the element to.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPrepend.R
#'
#' @family reshaping_functions
#'
#' @rdname xPrepend
#' @export

xPrepend <- MakeFun(function (val, coll) {



	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list(val)
	} else {
		as.list( c(list(val), coll) )
	}
})

#' @rdname xPrepend
#' @export

xPrepend_ <- MakeVariadic(xPrepend, 'coll')
