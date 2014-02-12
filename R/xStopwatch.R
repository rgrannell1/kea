
#' xStopwatch
#'
#' Create a function that returns true for a
#' preset time after creation.
#'
#' @param
#'    num a positive number. The number of seconds
#'    the function should return true for.
#'
#' @return
#'    A variadic function that ignores its arguments.
#'
#' @family time_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xStopwatch.R
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

	insist $ must_be_collection(num, invoking_call)
	num <- unit_to_value(as_typed_vector(num, 'numeric'))

	insist $ must_be_of_length(num, 1)
	insist $ must_be_grequal_than(num, 0, invoking_call)

	genesis <- Sys.time()

	function (...) {
		"a function returned by xStopwatch."
		""
		difftime(Sys.time(), genesis) < num
	}
}
