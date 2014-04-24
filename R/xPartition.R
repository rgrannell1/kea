
#' xPartition
#'
#' Divide elements in a collection into two collections
#' based on a predicate function.
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

	MACRO( Must $ Not_Be_Missing(pred) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(pred) )
	MACRO( Must $ Be_Collection(coll) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- vapply(coll, function (elem) isTRUE(pred(elem)), logical(1), USE.NAMES = False)

		true_ind <- !is.na(ind) & ind

		list(
			as.list(coll[true_ind]),
			as.list(coll[!true_ind]) )
	}
})

#' @rdname xPartition
#' @export

xPartition_ <- function (pred, ...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xPartition(pred, list(...))
}
