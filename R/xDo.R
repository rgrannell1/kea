
#' xDo
#'
#' Map (a possibly side-effectful) function over a
#' collection and discard the results.
#'
#' @param
#'    fn a unary function, usually side-effectful.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @template
#'    Variadic
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xDo.R
#'
#' @rdname xDo
#' @export

xDo <- MakeFun(function (fn, coll) {
	# function -> Collection any -> Null
	# apply a function to each element of a collection.
	# and discard the results.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		Null
	} else {

		try_hof({
			for (ith in seq_along(coll)) {
				fn( coll[[ith]] )
			}},
			invoking_call
		)

		invisible (Null)
	}
})

#' @rdname xDo
#' @export

xDo... <- function (fn, ...) {
	xDo(fn, list(...))
}
