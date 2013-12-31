
#' xStopwatch
#'
#' Create a function that returns true for a
#' preset time after creation.
#'
#' @section Uses:
#'     \code{xStopwatch} is used to measure the passage of time
#'     beyond a threshold. \code{xStopwatch} is used internally
#'     by Arrow's testing suite to check that tests are run for
#'     a certain amount of time, but no longer. By default,
#'     unit tests are run until a 0.1 second \code{xStopwatch} returns
#'     false. The tests then terminate.
#'
#' @param
#'    num a positive number.
#'
#' @return
#'    a variadic function that ignores its arguments.
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
		length(num) %in% 0:1, invoking_call,
		exclaim$must_have_length(num, 0:1))

	assert(
		num >= 0, invoking_call,
		exclaim$must_be_grequal_than(num, 0))

	genesis <- Sys.time()

	function (...) {
		difftime(Sys.time(), genesis) < num
	}
}
