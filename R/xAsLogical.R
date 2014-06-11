
#' xAsLogical
#'
#' Convert a collection of logicals to a logical vector.
#'
#' @section Type Signature:
#'     |logical| -> &lt;logical>
#'
#' @details
#'    \bold{xAsLogical} converts a list, pairlist or vector of
#'    length-one logical values to a logical vector. It does not attempt
#'    to convert non-logical collections to logical vectors.
#'
#' @param
#'    bools a collection of logical values.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A logical vector.
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsLogical.R
#'
#' @rdname xAsLogical
#' @export

xAsLogical <- MakeFun(function (bools) {

	MACRO( Must $ Be_Collection(bools) )

	bools <- as_typed_vector(bools, 'logical')

	if (length(bools) == 0) {
		logical(0)
	} else {
		as.logical(bools)
	}
})

#' @rdname xAsLogical
#' @export

xAsLogical_ <- MakeVariadic(xAsLogical, 'bools')
