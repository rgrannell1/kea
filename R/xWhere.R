
#' xWhere
#'
#' Return the indices for which a boolean vector is true.
#'
#' @section Type Signature:
#'     |logical| -> &lt;integer>
#'
#' @param
#'    bools a collection of boolean values. The collection to
#'    find the true elements in.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An integer vector.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero,
#'    or no match is found. NA values are treated as false.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xWhere.R
#'
#' @rdname xWhere
#' @export

xWhere <- MakeFun('xWhere', function (bools) {

	if (length(bools) == 0) {
		keep_names(integer(0), bools)
	} else {
		which(bools)
	}
})

#' @rdname xWhere
#' @export

xWhere_ <- MakeVariadic(xWhere, 'bools')
