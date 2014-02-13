
#' xAsLogical
#'
#' Convert a collection to a logical vector.
#'
#' @details
#'    \code{xAsLogical} converts a list, pairlist or vector of
#'    length-one logical values to a logical vector. It does not attempt
#'    to convert non-logical collections to logical vectors.
#'
#' @param
#'    bools a collection of logical values.
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
#'
#' @example
#'    inst/examples/example-xAsLogical.R
#'
#' @rdname xAsLogical
#' @export

xAsLogical <- function (bools) {
	# Collection logical -> Vector logical
	# convert a collection to a logical vector.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(bools)
	insist $ must_be_collection(bools, invoking_call)

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
