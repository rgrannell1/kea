
#' xReverse
#'
#' Reverse a collection.
#'
#' @param
#'    coll a collection. The collection to reverse.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of the same length as \bold{coll}
#'
#' @section Corner Cases:
#'    Reversing the empty list yields the empty list.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xReverse.R
#'
#' @rdname xReverse
#' @export

xReverse <- function (coll) {
	# Collection any -> [any]
	# reverse a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		as.list(rev(coll))
	}
}

#' @export

xReverse... <- function (...) {
	xReverse(list(...))
}
