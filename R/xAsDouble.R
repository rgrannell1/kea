
#' xAsDouble
#'
#' Convert a collection to a double vector.
#'
#' @section Type Signature:
#'     |number| -> <numeric>
#'
#' @details
#'    \code{xAsDouble} converts a list, pairlist or vector of
#'    length-one double precision numbers to a double vector. It does not attempt
#'    to convert non-double collections to double vectors.
#'
#' @param
#'    nums a collection of double values. A list, pairlist or vector
#'    of length-one double vectors to convert to a double vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A double vector.
#'
#' @section Corner Cases:
#'    Integer vectors are coerced to double vectors freely, since the
#'    difference is predominantly internal.
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsDouble.R
#'
#' @rdname xAsDouble
#' @export

xAsDouble <- MakeFun(function (nums) {

	MACRO( Must $ Not_Be_Missing(nums) )
	MACRO( Must $ Be_Collection(nums) )

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		double(0)
	} else {
		as.double(nums)
	}
})

#' @rdname xAsDouble
#' @export

xAsDouble_ <- MakeVariadic(xAsDouble, 'nums')
