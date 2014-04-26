
#' xWhere
#'
#' Return the indices for which a boolean vector is true.
#'
#' @section Type Signature:
#'     |logical| -> <integer>
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
#'    or no match is found. If the predicate returns a
#'    non-logical value an error is thrown. If an na value
#'    is returned by the predicate it is treated as a false value.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xWhere.R
#'
#' @rdname xWhere
#' @export

xWhere <- MakeFun(function (bools) {

	MACRO( Must $ Not_Be_Missing(bools) )
	MACRO( Must $ Be_Collection(bools) )

	bools <- as_typed_vector(bools, "logical")

	if (length(bools) == 0) {
		integer(0)
	} else {
		which(bools)
	}
})

#' @rdname xWhere
#' @export

xWhere_ <- MakeFun(function (...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xWhere(list(...))
})
