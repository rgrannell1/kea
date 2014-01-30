
#' xPack
#'
#' Remove all length-zero values from a collection.
#'
#' @details
#'    \bold{xPack} selects the non-empty elements from
#'    a collection. Zero-length vectors like \bold{integer(0)}
#'    and \bold{Null} are removed from the input collection.
#'
#' @param
#'    coll a collection. The collection to remove
#'    empty values from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the emty list if \bold{coll} is length-zero,
#'    or all elements in \bold{coll} are length-zero.
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xPack.R
#'
#' @rdname xPack
#' @export

xPack <- function (coll) {
	# Collection any -> [any]
	# remove all length-zero elements from a coll

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		coll[vapply(coll, length, integer(1)) != 0]
	}
}

#' @rdname xPack
#' @export

xPack... <- function (...) {
	xPack(list(...))
}
