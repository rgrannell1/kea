
#' xFlatMap
#'
#' Concatenate the results of applying a function
#' to each element of a collection.
#'
#' @section Type Signature:
#'     (any -> any) -> |any| -> [any]
#'
#' @details
#'     \bold{xFlatMap} is similar to \bold{xMap} but allows more
#'     fine-grained control of the output format. For example, elements
#'     can be removed (by returning NULL) or added (by returning a collection),
#'     or names can be added to the collection.
#'
#'     \bold{xFlatMap} can be used to emulate the behaviour of \bold{xSelect},
#'     allowing it to drop elements:
#'
#'     \code{odds <- xFlatMap(x := if (x \%\% 2) x, 1:10)}
#'
#'     \code{list(1, 3, 5, 7, 9)}
#'
#'     It may also be used to add new elements to a collection:
#'
#'     \code{xFlatMap(x := c(num = x, sqrt = sqrt(x)), 1:3)  )}
#'
#'     \code{list(num = 1, sqrt = 1, num = 2, sqrt = 1.41, num = 3, sqrt = 1.73)}
#'
#' @param
#'    fn a unary function. The function to map across a collection.
#'
#' @param
#'    coll a collection. The collection to map across.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'	  Returns the empty list if \bold{coll} is length-zero.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xFlatMap.R
#'
#' @rdname xFlatMap
#' @export

xFlatMap <- MakeFun(function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	if (length(coll) == 0) {
		# - this might not be needed.
		keep_names(list(), coll)
	} else {
		# -- this might need a more rigourous implementation.
		as.list( do.call(c,
			MACRO( Try_Higher_Order_Function( lapply(coll, fn)) ) ) )
	}
})

#' @rdname xFlatMap
#' @export

xFlatMap_ <- MakeVariadic(xFlatMap, 'coll')

