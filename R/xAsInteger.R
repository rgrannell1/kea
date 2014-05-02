
#' xAsInteger
#'
#' Convert a collection to a integer vector.
#'
#' @section Type Signature:
#'     |integer| -> <integer>
#'
#' @details
#'    \code{xAsInteger} converts a list, pairlist or vector of
#'    length-one integers to a integer vector. It does not attempt
#'    to convert non-integer collections to integer vectors.
#'
#' @param
#'    nums a collection of integer values. A list, pairlist or vector
#'    of length-one integer vectors to convert to a integer vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An integer vector.
#'
#' @section Corner Cases:
#'    Double vectors are not coerced to integer vectors:
#'    doubles can be decimal or infinite numbers, which
#'    would be lost upon coersion.
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsInteger.R
#'
#' @rdname xAsInteger
#' @export

xAsInteger <- MakeFun(function (nums) {

	MACRO( Must $ Not_Be_Missing(nums) )
	MACRO( Must $ Be_Collection(nums) )

	nums <- as_typed_vector(nums, 'integer')

	MACRO( Must $ Be_Whole(nums) )

	if (length(nums) == 0) {
		integer(0)
	} else {
		as.integer(nums)
	}
})

#' @rdname xAsInteger
#' @export

xAsInteger_ <- MakeVariadic(xAsInteger, 'nums')
