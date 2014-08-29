
#' xSwap
#'
#' Replace each occurence of a certain value in a collection with another value.
#'
#' @section Type Signature:
#'     any -> any -> [any] -> [any]
#'
#' @param
#'     val1 an arbitrary value. The value to replace.
#'
#' @param
#'     val2 an arbitrary value. The replacement value.
#'
#' @param
#'     coll a collection. The collection to replace elements in.
#'
#' @param
#'     ... see above.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSwap.R
#'
#' @section Corner Cases:
#'    The equality test that \bold{xSwap} distinguishes between numbers with very small
#'    differences (0.999999 != 1) and treats -0 as equal to 0.
#'
#' @template
#'    S-Experimental
#'
#' @rdname xSwap
#' @export

xSwap <- MakeFun('xSwap', function (val1, val2, coll) {

	if (length(coll) == 0) {
		keep_names(list(), coll)
	} else {
		lapply(coll, function (elem) {
			if (identical(elem, val1)) val2 else elem
		})
	}

})

#' @rdname xSwap
#' @export

xSwap_ <- MakeVariadic(xSwap, 'coll')
