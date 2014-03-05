

#' xDeepMap
#'
#' Recursively map a function into a nested collection,
#' preserving its structure.
#'
#' @param
#'    fn a unary function. A function to recursively apply
#'    into a collection.
#'
#' @param
#'    coll a collection. The collection to be mapped into.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list or pairlist.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDeepMap.R
#'
#' @rdname xDeepMap
#' @export

xDeepMap <- MakeFun(function (fn, coll) {
	# (any -> any) -> Recursive any -> [any]
	# Map a function into a nested collection,
	# preserving its structure.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(fn) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(fn) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	recur <- function (xs) {
		# recurse into a collection.

		if (is.list(xs) || is.pairlist(xs)) {
			lapply(xs, recur)
		} else {
			fn(xs)
		}
	}

	try_hof(
		recur(as.list(coll)), invoking_call)
})

#' @rdname xDeepMap
#' @export

xDeepMap... <- function (fn, ...) {
	xDeepMap(fn, list(...))
}
