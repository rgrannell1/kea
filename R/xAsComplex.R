
#' xAsComplex
#'
#' Convert a collection to a complex vector.
#'
#' @details
#'    \code{xAsComplex} converts a list, pairlist or vector of
#'    length-one complex numbers to a complex vector. It does not attempt
#'    to convert non-complex collections to complex vectors.
#'
#' @param
#'    ims a collection of complex values. A list, pairlist or vector
#'    of length-one complex vectors to convert to a complex vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A complex vector.
#'
#' @template
#'    Variadic
#'
#' @rdname xAsComplex
#' @export

xAsComplex <- function (ims) {
	# Collection integer -> Vector integer
	# convert a collection to a integer vector.

	invoking_call <- sys.call()

	assert(
		!missing(ims), invoking_call,
		exclaim$parametre_missing(ims))

	insist$must_be_collection(ims, invoking_call)

	as_typed_vector(ims, 'complex')

}

#' @rdname xAsComplex
#' @export

xAsComplex... <- function (...) {
	xAsComplex(list(...))
}
