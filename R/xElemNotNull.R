
#' xElemNotNull
#'
#' Is an element of a collection null?
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns logical(0) if coll is itself Null.
#'
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemNotNull.R
#'
#' @rdname xElemNotNull
#' @export

xElemNotNull <- function (coll) {
	# collection any -> vector Boolean
	# are the elements of a collection not null?

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		# empty pairlist - an odd corner case.
		logical(0)
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- !identical(coll[[ith]], Null)
		}
		res
	}
}

#' @rdname xElemNotNull
#' @export

xElemNotNull... <- function (...) {
	xElemNotNull(list(...))
}
