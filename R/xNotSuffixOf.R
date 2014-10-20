
#' xNotSuffixOf
#'
#' Is a collection a suffix of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> <logical>
#'
#' @param
#'    coll1 a collection. The suffix to test for absense of.
#'
#' @param
#'    coll2 a collection. The collection to test for a suffix.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If the first collection is longer than the second, True is returned.
#'    If either collection is length-zero, \bold{logical(0)} is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotSuffixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xNotSuffixOf
#' @export

xNotSuffixOf <- MakeFun(function (coll1, coll2)
	cNotSuffixOf(coll1, coll2)
)

#' @rdname xNotSuffixOf
#' @export

xNotSuffixOf_ <- MakeVariadic(xNotSuffixOf, 'coll2')
