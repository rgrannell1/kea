
#' xAsDouble
#'
#' Convert a collection to a double vector.
#'
#' @details
#'    \code{xAsDouble} converts a list, pairlist or vector of
#'    length-one double precision numbers to a double vector. It does not attempt
#'    to convert non-double collections to double vectors.
#'
#' @param
#'    nums a collection of boolean values.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A double vector.
#'
#' @template
#'    Variadic
#'
#' @rdname xAsDouble
#' @export

xAsDouble <- function (nums) {
	# Collection integer -> Vector integer
	# convert a collection to a integer vector.

	invoking_call <- sys.call()

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	insist$must_be_collection(nums, invoking_call)

	nums <- as_typed_vector(nums, 'numeric')

	if (length(nums) == 0) {
		double(0)
	} else {
		as.double(nums)
	}
}

#' @rdname xAsDouble
#' @export

xAsDouble... <- function (...) {
	xAsDouble(list(...))
}
