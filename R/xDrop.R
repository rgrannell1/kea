
#' xDrop
#'
#' Take several elements from the head of a collection.
#'
#' @section Type Signature:
#'     |number| -> |any| -> [any]
#'
#' @param
#'      num a nonnegative whole number. The number of elements to
#'      return from the input collection.
#'
#' @param
#'      coll a collection. The collection to subset.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    If \bold{coll} is empty the empty list is returned.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDrop.R
#'
#' @rdname xDrop
#' @export

xDrop <- MakeFun(function (num, coll) {


	num <- unit_to_value(as_atom(num, 'numeric'))

	MACRO( Must_Be_Whole(num) )
	MACRO( Must_Be_Between(num, 0, Inf))

	if (length(coll) == 0 || num >= length(coll)) {
	 	list()
	} else {
		as.list(coll)[(num + 1) : length(coll)]
	}
})

#' @rdname xDrop
#' @export

xDrop_ <- MakeVariadic(xDrop, 'coll')
