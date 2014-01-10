
#' xAsLogical
#'
#' Convert a collection to a logical vector.
#'
#' @param
#'    bools a collection of boolean values.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A logical vector.
#'
#' @template
#'    Variadic
#'
#' @rdname xAsLogical
#' @export

xAsLogical <- function (bools) {
	# Collection logical -> Vector logical
	# convert a collection to a logical vector.

	invoking_call <- sys.call()

	assert(
		!missing(bools), invoking_call,
		exclaim$parametre_missing(bools))

	assert(
		is_collection(bools), invoking_call,
		exclaim$must_be_collection(
			bools, summate(bools)) )

	bools <- as_typed_vector(bools, 'logical')

	if (length(bools) == 0) {
		logical(0)
	} else {
		as.logical(bools)
	}
}

#' @rdname xAsLogical
#' @export

xAsLogical... <- function (...) {
	xAsLogical(list(...))
}
