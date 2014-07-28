
#' xDeepMap
#'
#' Recursively map a function into a nested collection,
#' preserving its structure.
#'
#' @section Type Signature:
#'     (any -> any) -> |any| -> [any]
#'
#' @details
#'     \bold{xDeepMap} is currently recursive, and as such will cause a
#'     stack overflow for large inputs. Future versions of Kiwi may include
#'     a more stable algorithm for \bold{xDeepMap}.
#'
#' @param
#'    fn a unary function. A function to recursively apply
#'    into a collection.
#'
#' @param
#'    coll a collection. The collection to be mapped into.
#'
#' @param
#'    ... see above.
#'
#' @section Corner Cases:
#'    xDeepMap preseves collection names.
#'
#' @return
#'    A list or pairlist.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDeepMap.R
#'
#' @rdname xDeepMap
#' @export

xDeepMap <- MakeFun('xDeepMap', function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	rapply(coll, function (elem) {

		 MACRO( Try_Higher_Order_Function( fn(elem) ) )

	}, how = 'list')

})

#' @rdname xDeepMap
#' @export

xDeepMap_ <- MakeVariadic(xDeepMap, 'coll')
