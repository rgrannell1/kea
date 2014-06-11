
#' xPartition
#'
#' Divide elements in a collection into two collections
#' based on a predicate function.
#'
#' @section Type Signature:
#'     (any -> &lt;logical>) -> |any| -> [[any]]
#'
#' @param
#'    pred a predicate. The predicate function with which to partition
#'    the input collection.
#'
#' @param
#'    coll a collection. The collection to partition into
#'    two collections.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    Returns two lists; a list of elements in a collection for which a
#'    predicate returns true, and a list of elements in a collection for which
#'    a predicate returns false or na.
#'
#' @section Corner Cases:
#'	  If \bold{coll} is empty the empty list is returned. If all the
#'	  elements return only true/only false, then one of two sublists will be the
#'	  empty list.
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPartition.R
#'
#' @rdname xPartition
#' @export

xPartition <- MakeFun(function (pred, coll) {

	if (length(coll) == 0) {
		list()
	} else {

		ind <- vapply(coll, function (elem) {
			isTRUE(pred(elem))
		}, logical(1), USE.NAMES = False)

		true_ind <- !elem_is_na(ind) & ind

		list(
			as.list(coll[true_ind]),
			as.list(coll[!true_ind]) )
	}
})

#' @rdname xPartition
#' @export

xPartition_ <- MakeVariadic(xPartition, 'coll')
