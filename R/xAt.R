
#' xAt
#'
#' Select a collection at certain indices.
#'
#'
#'
#'
#'
#'
#'
#' @export

xAt <- function (nums, coll) {
	# Vector numbers -> Collection any -> Collection any
	# select elements of a collection using indices.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		!missing(nums), invoking_call,
		exclaim$parametre_missing(nums))

	insist $ must_be_collection(coll, invoking_call)
	insist $ must_be_collection(nums, invoking_call)

	nums <- to_value_unit(as_typed_vector(nums, 'numeric'))

	insist $ must_be_whole(nums, invoking_call)
	insist $ max_must_be_less_than_length_of(nums, coll, invoking_call)

	coll[nums]
}

#' @rdname xAt
#' @export

xAt... <- function (nums, ...) {
	xAt(nums, list(...))
}
