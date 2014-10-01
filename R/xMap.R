
#' xMap
#'
#' Apply a function to each element of a collection.
#'
#' @section Type Signature:
#'     (any -> any) -> |any| -> [any]
#'
#' @details
#'     \bold{xMap} is probably the most commonly used higher-order function.
#'     It is also one of the easier ones to understand. \bold{xMap} takes a function
#'     that works on a single value (like \bold{isTRUE}) and a collection of such values.
#'     It then calls that function on each element of a collection.
#'
#'     \code{xMap(isTRUE, list(True, False, 10))}
#'
#'     is essentially transformed to
#'
#'     \code{list(isTRUE(True), isTRUE(False), isTRUE(10))}
#'
#'     which result in
#'
#'     \code{list(True, False, False)}
#'
#' @param
#'    fn a unary function. The function to modify each
#'    element of a collection with.
#'
#' @param
#'    coll a collection. The collection to be modified.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
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
#'    inst/examples/example-xMap.R
#'
#' @rdname xMap
#' @export

xMap <- MakeFun(function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	if (length(coll) == 0) {
		keep_names(list(), coll)
	} else {
		MACRO( Try_Higher_Order_Function( lapply(coll, fn) ) )
	}

})

#' @rdname xMap
#' @export

xMap_ <- MakeVariadic(xMap, 'coll')
