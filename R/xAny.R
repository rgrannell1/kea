
#' xAny
#'
#' Is a predicate true for any element of a collection?
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
#'    inst/examples/example-xAny.R
#'
#' @rdname xAny
#' @export

xAny <- function (pred, coll) {

	insist $ must_not_be_missing(pred)
	insist $ must_not_be_missing(coll)

	insist $ must_be_fn_matchable(pred, invoking_call)

    insist $ must_be_collection(coll, invoking_call)
    insist $ must_be_collection_of_collections(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		logical(0)
	} else {
		any(vapply(coll, pred, logical(1), USE.NAMES = False))
	}
}

#' @rdname xAny
#' @export

xAny... <- function (pred, ...) {
	xAny(pred, list(...))
}

