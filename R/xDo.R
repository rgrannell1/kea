
#' xDo
#'
#' Map (a possibly side-effectful) function over a
#' collection and discard the results.
#'
#' @section Type Signature:
#'     (any -> any) -> |any| -> {}
#'
#' @details
#'     \bold{xDo} is superficially similar to \bold{xMap}, in that
#'     it applies a function to each element of a collection. However
#'     \bold{xDo} does not accumulate a result, so it is more memory-efficient
#'     and faster at executing side-effectful functions than \bold{xMap}.
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

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		Null
	} else {

		for (ith in seq_along(coll)) {
			fn( coll[[ith]] )
		}

		invisible (Null)
	}
})

#' @rdname xDo
#' @export

xDo_ <- MakeVariadic(xDo, 'coll')
