
#' xAsComplex
#'
#' Convert a collection of complex values to a complex vector.
#'
#' @section Type Signature:
#'     |complex| -> <complex>
#'
#' @details
#'    \bold{xAsComplex} converts a list, pairlist or vector of
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
#'    An complex vector.
#'
#' @template
#'    Variadic
#'
#' @section Corner Cases:
#'     xAsComplex will throw an error if its input is
#'     not a collection of complex numbers. All length-zero collections
#'     can be converted to complex.
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsComplex.R
#'
#' @template S-Uncertain
#' @rdname xAsComplex
#' @export

xAsComplex <- MakeFun(function (ims)
	ims
)

#' @rdname xAsComplex
#' @export

xAsComplex_ <- MakeVariadic(xAsComplex, 'ims')
