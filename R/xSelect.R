
#' xSelect
#'
#' Include all elements from a collection matching a predicate.
#'
#' @section Type Signature:
#'     (any -> <logical>) -> |any| -> [any]
#'
#' @details
#'    \bold{xSelect} applies a predicate function
#'    such as \bold{is.integer} or \bold{is.null} to each element
#'    of a collection. The elements for which the predicate
#'    returned true are kept, and the remaining elements are
#'    removed from the collection.
#'
#' @param
#'    pred a predicate. The function used to test each element of
#'    the input collection.
#'
#' @param
#'    coll a collection. The collection to remove elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero,
#'    or no match is found. If the predicate returns a
#'    non-logical value an error is thrown. If an na value
#'    is returned by the predicate it is treated as a false value.
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSelect.R
#'
#' @rdname xSelect
#' @export

xSelect <- MakeFun(function (pred, coll) {

	MACRO( Must_Have_Arity(pred, 1) )

#	ind <- vapply(coll, function (elem) {
#
#		is_match <- MACRO( Try_Higher_Order_Function(pred(elem)) )
#
#		MACRO(Must_Be_Flag(is_match, pred))
#
#		is_match
#
#	}, logical(1))

	ind <- MACRO( Try_Higher_Order_Function(
		vapply(coll, function (elem) {

			is_match <- pred(elem)

			MACRO(Must_Be_Flag(is_match, pred))

			is_match

		}, logical(1))
	) )

	as.list( coll[ !elem_is_na(ind) & ind ] )
})

#' @rdname xSelect
#' @export

xSelect_ <- MakeVariadic(xSelect, 'coll')
