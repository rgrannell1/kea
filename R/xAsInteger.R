
#' xAsInteger
#'
#' Unbox a collection of integers to a integer vector.
#'
#' @section Type Signature:
#'     |integer| -> <integer>
#'
#' @details
#'    \bold{xAsInteger} unboxes a list, pairlist or vector of
#'    length-one integers to a integer vector. It does not attempt
#'    to convert non-integer collections to integer vectors.
#'
#' @param
#'    ints a collection of integer values. A list, pairlist or vector
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
#'    would be lost upon coercion. NA values can always be unboxed to any type.
#'
#' @template
#'    Variadic
#'
#' @family container_conversion_functions
#' @family unboxing_functions
#'
#' @example
#'    inst/examples/example-xAsInteger.R
#'
#' @template S-Uncertain
#' @rdname xAsInteger
#' @export

xAsInteger <- MakeFun(function (ints)
	ints
)

#' @rdname xAsInteger
#' @export

xAsInteger_ <- MakeVariadic(xAsInteger, 'ints')
