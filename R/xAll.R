
#' xAll
#'
#' Is a predicate true for all elements of a collection?
#'
#' @param
#'    pred a predicate. The function used to test each element of
#'    the input collection.
#'
#' @param
#'    coll a collection. The collection to test each element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    If coll is length zero then logical(0) is returned.
#'
#' @family quantifier_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAll.R
#'
#' @rdname xAll
#' @export

xAll <- function (pred, coll) {

	insist $ must_not_be_missing(pred)
	insist $ must_not_be_missing(coll)

	insist $ must_be_fn_matchable(pred, invoking_call)

    insist $ must_be_collection(coll, invoking_call)
    insist $ must_be_collection_of_collections(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		logical(0)
	} else {
		all(vapply(coll, pred, logical(1), USE.NAMES = False))
	}
}

#' @rdname xAll
#' @export

xAll... <- function (pred, ...) {
	xAll(pred, list(...))
}
