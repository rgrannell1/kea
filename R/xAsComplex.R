
#' xAsComplex
#'
#' Convert a collection to a complex vector.
#'
#' @param
#'    comps a collection of complex values.
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

xAsComplex <- function (comps) {
	# Collection integer -> Vector integer
	# convert a collection to a integer vector.

	invoking_call <- sys.call()

	assert(
		!missing(comps), invoking_call,
		exclaim$parametre_missing(comps))

	assert(
		is_collection(comps), invoking_call,
		exclaim$must_be_collection(
			comps, summate(comps)) )

	as_typed_vector(comps, 'complex')

}

#' @rdname xAsComplex
#' @export

xAsComplex... <- function (...) {
	xAsComplex(list(...))
}
