
#' xAsDouble
#'
#' Convert a collection to a double vector.
#'
#' @param
#'    nums a collection of boolean values.
#'
#' @param
#'    ... see above.
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

	assert(
		is_collection(nums), invoking_call,
		exclaim$must_be_collection(
			nums, summate(nums)) )

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
