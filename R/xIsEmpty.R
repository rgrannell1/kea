
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
#' @family value_testing_functions
#'
#' @rdname xIsEmpty
#' @export

xIsEmpty <- MakeFun(function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(coll) )

	length(coll) == 0
})

#' @rdname xIsEmpty
#' @export

xIsEmpty... <- function (...) {
	xIsEmpty(list(...))
}
