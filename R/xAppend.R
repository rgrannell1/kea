
#' xAppend
#'
#' Add a value to the end of a collection.
#'
#' @section Type Signature:
#'    any -> |any| -> [any]
#'
#' @param
#'    val an arbitrary value. The value to append.
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
#'    inst/examples/example-xAppend.R
#'
#' @family reshaping_functions
#'
#' @rdname xAppend
#' @export

xAppend <- MakeFun(function (val, coll) {

	MACRO( Fix(xAppend, val, coll) )

	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list(val)
	} else {
		as.list( c(coll, list(val)) )
	}
})

#' @rdname xAppend
#' @export

xAppend_ <- MakeVariadic(xAppend, 'coll')
