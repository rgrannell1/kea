
#' xIsEmpty
#'
#' Is a collection length-zero?
#'
#' @param
#'    val an arbitrary value. A value to check for being length zero.
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
#'    inst/examples/example-xIsEmpty.R
#'
#' @rdname xIsEmpty
#' @export

xIsEmpty <- function (val) {
	# Collection -> boolean
	# is collection length == 0?

	invoking_call <- sys.call()

	insist $ must_not_be_missing(val)
	insist $ must_be_collection(val, invoking_call)

	length(val) == 0
}

#' @rdname xIsEmpty
#' @export

xIsEmpty... <- function (...) {
	xIsEmpty(list(...))
}
