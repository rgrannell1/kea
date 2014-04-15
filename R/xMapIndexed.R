
#' xMapIndexed
#'
#' Apply a binary function to each element of a
#' collection and its index.
#'
#' @param
#'    fn a binary function. A function that takes a
#'    value in \bold{coll} as its left argument and a
#'    an index as its right argument.
#'
#' @param
#'    coll a collection. The collection to apply a function to.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list is \bold{coll} is length-zero.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMapIndexed.R
#'
#' @rdname xMapIndexed
#' @export

xMapIndexed <- MakeFun(function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]
	# Map over a collection, also passing each elements
	# index.

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {

		Map(
			function (ind) {
				fn( coll[[ind]], ind )
			},
			seq_along(coll)
		)
	}
})

#' @rdname xMapIndexed
#' @export

xMapIndexed... <- function (fn, ...) {
	xMapIndexed(fn, list(...))
}
