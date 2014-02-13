
#' xNotEmpty
#'
#' Is a collection length-zero?
#'
#' @param
#'    coll a collection. The value to test for
#'    having non-zero length.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotEmpty.R
#'
#' @rdname xNotEmpty
#' @export

xNotEmpty <- function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)
	insist $ must_be_collection(coll, invoking_call)

	length(coll) != 0
}

#' @rdname xNotEmpty
#' @export

xNotEmpty... <- function (...) {
	xNotEmpty(list(...))
}
