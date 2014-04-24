
#' xReverse
#'
#' Reverse the order of elements in a collection.
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

xReverse <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {
		as.list(rev(coll))
	}
})

#' @rdname xReverse
#' @export

xReverse_ <- function (...) {
	xReverse(list(...))
}
