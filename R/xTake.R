
#' xTake
#'
#' Take several elements from the head of a collection.
#'
#' @section Type Signature:
#'    <number> -> |any| -> |any|
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
#'    inst/examples/example-xTake.R
#'
#' @rdname xTake
#' @export

xTake <- MakeFun(function (num, coll) {

	MACRO( Must $ Not_Be_Missing(num) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(num) )
	MACRO( Must $ Be_Collection(coll) )

	num <- unit_to_value(as_atom(num, 'numeric'))

	MACRO( Must $ Be_Between(num, 0, Inf))
	MACRO( Must $ Be_Whole(num) )

	if (length(coll) == 0 || num == 0) {
		list()
	} else if (is.infinite(num)) {
		as.list(coll)
	} else {
		as.list(coll)[seq_len( min(num, length(coll)) )]
	}
})

#' @rdname xTake
#' @export

xTake_ <- MakeVariadic(xTake, 'coll')
