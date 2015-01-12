
#' xChunk
#'
#' Divide a collection into segments of fixed length.
#'
#' @section Type Signature:
#'     |numeric| -> |any| -> [[any]]
#'
#' @param
#'    num a nonnegative whole number. The desired
#'    length of each group of elements.
#'
#' @param
#'    coll a collection. The collections to divide into groups.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of lists.
#'
#' @section Corner Cases:
#'    The final list in the return value will have less than \bold{num}
#'    elements if \code{length(coll)} is not evenly divisible by \bold{num}.
#'    if \bold{coll} or \bold{num} is length-zero, the empty list is returned.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @template
#'    C++
#'
#' @example
#'    inst/examples/example-xChunk.R
#'
#' @rdname xChunk
#' @export

xChunk <- MakeFun(function (num, coll) {

	if (length(num) == 0) {
		list()
	} else {

		MACRO( Must_Be_Whole(num) )
		MACRO( Must_Be_Between(num, 1, Inf))

		cChunk(num, coll)

	}

})

#' @rdname xChunk
#' @export

xChunk_ <- MakeVariadic(xChunk, 'coll')
