
#' xAsInteger
#'
#' Convert a collection to a integer vector.
#'
#' @param
#'    nums a collection of boolean values.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An integer vector.
#'
#' @template
#'    Variadic
#'
#' @rdname xAsInteger
#' @export

xAsInteger <- function (nums) {
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

	assert(
		all(round(nums) == nums), invoking_call,
		exclaim$must_be_wholes(
			nums, summate(nums)) )

	if (length(nums) == 0) {
		integer(0)
	} else {
		as.integer(nums)
	}
}

#' @rdname xAsInteger
#' @export

xAsInteger... <- function (...) {
	xAsInteger(list(...))
}
