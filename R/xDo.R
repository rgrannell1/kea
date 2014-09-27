
#' xDo
#'
#' Map (a possibly side-effectual) function over a
#' collection and discard the results.
#'
#' @section Type Signature:
#'     (any -> any) -> |any| -> { }
#'
#' @details
#'     \bold{xDo} is superficially similar to \bold{xMap}, in that
#'     it applies a function to each element of a collection. However
#'     \bold{xDo} does not accumulate a result, so it is more memory-efficient
#'     and faster at executing side-effectual functions than \bold{xMap}.
#'
#' @param
#'    fn a unary function, usually side-effectual.
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
#' @section Corner Cases:
#'    Does not call \bold{fn} when given an empty list.
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

	MACRO( Must_Have_Arity(fn, 1) )

	if (length(coll) == 0) {
		invisible (Null)
	} else {

		for ( ith in seq_len(length(coll)) ) {
			 MACRO( Try_Higher_Order_Function( fn( coll[[ith]] ) ) )
		}

		invisible (Null)
	}
})

#' @rdname xDo
#' @export

xDo_ <- MakeVariadic(xDo, 'coll')
