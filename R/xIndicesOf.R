
#' xIndicesOf
#'
#' Get the indices of a collection.
#'
#' @section Type Signature:
#'     |any| -> <integer>
#'
#' @param
#'     coll a collection. The collection to get the indices of.
#'
#' @param
#'     ... see above.
#'
#' @return
#'     an integer vector.
#'
#' @section Corner Cases:
#'     Returns \bold{integer(0)} when \bold{coll} is length-zero.
#'
#' @family key_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIndicesOf.R
#'
#' @rdname xIndicesOf
#' @export

xIndicesOf <- MakeFun('xIndicesOf', function (coll) {

	if (length(coll) == 0) {
		integer(0)
	} else {
		seq_len(length(coll))
	}
})

#' @rdname xIndicesOf
#' @export

xIndicesOf_ <- MakeVariadic(xIndicesOf, 'coll')
