
#' xAsRaw
#'
#' Convert a collection to a raw vector.
#'
#' @details
#'    \code{xAsRaw} converts a list, pairlist or vector of
#'    length-one raw numbers to a raw vector. It does not attempt
#'    to convert non-raw collections to raw vectors.
#'
#' @param
#'    raws a collection of raw values.
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
#' @rdname xAsRaw
#' @export

xAsRaw <- function (raws) {
	# Collection integer -> Vector integer
	# convert a collection to a integer vector.

	invoking_call <- sys.call()

	assert(
		!missing(raws), invoking_call,
		exclaim$parametre_missing(raws))

	insist$must_be_collection(raws, invoking_call)

	as_typed_vector(raws, 'raw')

}

#' @rdname xAsRaw
#' @export

xAsRaw... <- function (...) {
	xAsRaw(list(...))
}
