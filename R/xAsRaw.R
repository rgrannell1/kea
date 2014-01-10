
#' xAsRaw
#'
#' Convert a collection to a raw vector.
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

	assert(
		is_collection(raws), invoking_call,
		exclaim$must_be_collection(
			raws, summate(raws)) )

	as_typed_vector(raws, 'raw')

}

#' @rdname xAsRaw
#' @export

xAsRaw... <- function (...) {
	xAsRaw(list(...))
}
