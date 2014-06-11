
#' xReject
#'
#' Remove all elements from a collection matching a predicate.
#'
#' @section Type Signature:
#'     (any -> &lt;logical>) -> |any| -> [any]
#'
#' @details
#'    \bold{xReject} applies a predicate function
#'    such as \bold{is.integer} or \bold{is.null} to each element
#'    of a collection. The elements for which the predicate
#'    returned false are kept, and the remaining elements are
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
#'    inst/examples/example-xReject.R
#'
#' @rdname xReject
#' @export

xReject <- MakeFun(function (pred, coll) {

	MACRO( Must $ Be_Fn_Matchable(pred) )
	MACRO( Must $ Be_Collection(coll) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- vapply(coll, function (elem) {
			isTRUE(pred(elem))
		}, logical(1), USE.NAMES = False)

		as.list( coll[elem_is_na(ind) | !ind ] )
	}
})

#' @rdname xReject
#' @export

xReject_ <- MakeVariadic(xReject, 'coll')
