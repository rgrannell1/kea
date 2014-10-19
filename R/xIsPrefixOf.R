
#' xIsPrefixOf
#'
#' Is a collection a prefix of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> <logical>
#'
#' @param
#'    coll1 a collection. The prefix to test for.
#'
#' @param
#'    coll2 a collection. The collection to test for a prefix.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If the first collection is longer than the second, False is returned.
#'    If either collection is length-zero, \bold{logical(0)} is returned.
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsPrefixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xIsPrefixOf
#' @export

xIsPrefixOf <- MakeFun(function (coll1, coll2)
	cIsPrefixOf(coll1, coll2)
)

#' @rdname xIsPrefixOf
#' @export

xIsPrefixOf_ <- MakeVariadic(xIsPrefixOf, 'coll2')
