
#' xStopwatch
#'
#' Create a function that returns true for a
#' preset time after creation.
#'
#' @param
#'    num a positive number.
#'
#' @return
#'    A variadic function that ignores its arguments.
#'
#' @family time_functions
#'
#' @rdname xStopwatch
#' @export

xStopwatch <- function (num) {
	# integer -> function
	# create a timer predicate function.

	invoking_call <- sys.call()

	assert(
		!missing(num), sys.call(),
		exclaim$parametre_missing(num))

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) == 1, invoking_call,
		exclaim$must_have_length(
			num, 1, summate(num)) )

	assert(
		num >= 0, invoking_call,
		exclaim$must_be_grequal_than(
			num, 0, summate(num)) )

	genesis <- Sys.time()

	function (...) {
		"a function returned by xStopwatch."
		""
		difftime(Sys.time(), genesis) < num
	}
}
