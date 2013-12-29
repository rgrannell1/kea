#' xReverse
#'
#' Reverse a collection.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a list of the same length as \code{coll}
#'
#' @section Corner Cases:
#'    reversing the empty list yields the empty list.
#'
#' @family collection_functions
#'
#' @family reshaping_functions
#'
#' @family variadic_functions
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
	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

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
