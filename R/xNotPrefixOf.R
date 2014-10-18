
#' xNotPrefixOf
#'
#' Is a collection not a prefix of another?
#'
#' @section Type Signature:
#'     |any| -> |any| -> <logical>
#'
#' @param
#'    coll1 a collection. The prefix to test for absense of.
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
#'    If the first collection is longer than the second, True is returned.
#'    If either collection is length-zero, \bold{logical(0)} is returned.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotPrefixOf.R
#'
#' @family value_testing_functions
#'
#' @rdname xNotPrefixOf
#' @export

xNotPrefixOf <- MakeFun(function (coll1, coll2) {
	cNotPrefixOf(coll1, coll2)
})

#' @rdname xNotPrefixOf
#' @export

xNotPrefixOf_ <- MakeVariadic(xNotPrefixOf, 'coll2')
