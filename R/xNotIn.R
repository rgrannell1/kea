
#' xNotIn
#'
#' Check if a collection doesn't contain a value.
#'
#' @section Type Signature:
#'     any -> |any| -> <logical>
#'
#' @param
#'    val an arbitrary value. The value to test for
#'    non-membership in a collection.
#'
#' @param
#'    coll a collection. The collection to test elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    The equality check used by \bold{xIsIn} has the same corner cases as \bold{xIs};
#'    equal integers and doubles are treated as different numbers, -0 is equal to +0.
#'    Returns \bold{logical(0)} when \bold{coll} is empty.
#'
#' @family set_functions
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotIn.R
#'
#' @rdname xNotIn
#' @export

xNotIn <- MakeFun(function (val, coll) {
	cNotIn(val, coll)
})

#' @rdname xNotIn
#' @export

xNotIn_ <- MakeVariadic(xNotIn, 'coll')
