
#' xAsComplex
#'
#' Convert a collection to a complex vector.
#'
#' @details
#'    \code{xAsComplex} converts a list, pairlist or vector of
#'    length-one complex values to a complex vector.
#'
#' @param
#'    ims a collection of complex values. A list, pairlist or vector
#'    of length-one complex vectors to convert to a complex vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An integer vector.
#'
#' @section Corner Cases:
#'    Corner
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xAsComplex.R
#'
#' @rdname xAsComplex
#' @export

xAsComplex <- function (ims) {
	# Collection integer -> Vector integer
	# convert a collection to a integer vector.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(ims)
	insist $ must_be_collection(ims, invoking_call)

	as_typed_vector(ims, 'complex')

}

#' @rdname xAsComplex
#' @export

xAsComplex... <- function (...) {
	xAsComplex(list(...))
}
