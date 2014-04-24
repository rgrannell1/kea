

#' xDeepMap
#'
#' Recursively map a function into a nested collection,
#' preserving its structure.
#'
#' @details
#'     \bold{xDeepMap} is currently recursive, and as such will cause a
#'     stack overflow for large inputs. Future versions of Arrow may include
#'     a more stable algorithm for \bold{xDeepMap}.
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

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	recur <- function (xs) {
		# recurse into a collection. TODO-non-recursive form.

		if (is.recursive(xs)) {
			# this recursively converts from pairlist to list.
			lapply(xs, recur)
		} else {
			fn(xs)
		}
	}

	recur(as.list(coll))
})

#' @rdname xDeepMap
#' @export

xDeepMap_ <- function (fn, ...) {
	xDeepMap(fn, list(...))
}
