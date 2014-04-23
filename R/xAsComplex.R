
#' xAsComplex
#'
#' Convert a collection to a complex vector.
#'
#' @section Type Signature:
#'     |complex| -> <complex>
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
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsComplex.R
#'
#' @rdname xAsComplex
#' @export

xAsComplex <- MakeFun(function (ims) {

	MACRO( Must $ Not_Be_Missing(ims) )
	MACRO( Must $ Be_Collection(ims) )

	as_typed_vector(ims, 'complex')
})

#' @rdname xAsComplex
#' @export

xAsComplex... <- function (...) {
	xAsComplex(list(...))
}
