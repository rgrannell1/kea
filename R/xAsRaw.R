
#' xAsRaw
#'
#' Convert a collection of raw values to a raw vector.
#'
#' @section Type Signature:
#'     |raw| -> &lt;raw>
#'
#' @details
#'    \bold{xAsRaw} converts a list, pairlist or vector of
#'    length-one raw numbers to a raw vector. It does not attempt
#'    to convert non-raw collections to raw vectors.
#'
#' @param
#'    raws a collection of raw values. A list, pairlist or vector
#'    of length-one raw vectors to convert to a raw vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A raw vector.
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsRaw.R
#'
#' @rdname xAsRaw
#' @export

xAsRaw <- MakeFun(function (raws) {

	MACRO( Must $ Not_Be_Missing(raws) )
	MACRO( Must $ Be_Collection(raws) )

	as_typed_vector(raws, 'raw')

})

#' @rdname xAsRaw
#' @export

xAsRaw_ <- MakeVariadic(xAsRaw, 'raws')
