
#' xNotEmpty
#'
#' Is a collection not length-zero?
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
#' @family value_testing_functions
#'
#' @rdname xNotEmpty
#' @export

xNotEmpty <- MakeFun(function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	isTRUE(length(coll) != 0)
})

#' @rdname xNotEmpty
#' @export

xNotEmpty... <- function (...) {
	xNotEmpty(list(...))
}
