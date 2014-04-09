
#' xOneOf
#'
#' Select a random value from a collection.
#'
#' @param
#'      coll a collection. The collection to take a value from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A value from \bold{coll}.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xOneOf.R
#'
#' @family reshaping_functions
#'
#' @rdname xOneOf
#' @export

xOneOf <- MakeFun(function (coll) {
	# collectionction any -> any

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		coll
	} else {
		ind <- sample.int(length(coll), 1)
		coll[[ind]]
	}
})

#' @rdname xOneOf
#' @export

xOneOf... <- function (...) {
	xOneOf(list(...))
}
