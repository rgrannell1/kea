
#' xAsDouble
#'
#' Unbox a collection of doubles or integers to a double vector.
#'
#' @section Type Signature:
#'     |number| -> <numeric>
#'
#' @details
#'    \bold{xAsDouble} unboxes a list, pairlist or vector of
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
#'    difference is predominantly internal. NA values can always be unboxed to any type.
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#' @family unboxing_functions
#'
#' @example
#'    inst/examples/example-xAsDouble.R
#'
#' @template S-Uncertain
#' @rdname xAsDouble
#' @export

xAsDouble <- MakeFun(function (nums)
	nums
)

#' @rdname xAsDouble
#' @export

xAsDouble_ <- MakeVariadic(xAsDouble, 'nums')
