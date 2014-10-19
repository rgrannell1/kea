
#' xRelate
#'
#' Map a function over each element of a collection, keeping both the element and the result.
#'
#' @section Type Signature:
#'     (any -> any) -> |any| -> [[any, any]]
#'
#' @param
#'    fn a unary function. The function to apply to each element of the collection.
#'
#' @param
#'    coll a collection. The collection to be mapped over.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of two-element lists.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xRelate.R
#'
#' @template
#'     S-Experimental
#'
#' @rdname xRelate
#' @export

xRelate <- MakeFun(function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	if (length(coll) == 0)
		keep_names(list(), coll)
	else

		MACRO( Try_Higher_Order_Function( lapply(coll, function (elem) {
			list(elem, fn(elem))
		}) ) )

})

#' @rdname xRelate
#' @export

xRelate_ <- MakeVariadic(xRelate, 'coll')
