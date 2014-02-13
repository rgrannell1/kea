
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

	insist $ must_not_be_missing(num)

	insist $ must_be_collection(num, invoking_call)
	num <- unit_to_value(as_atom(num, 'numeric'))

	insist $ must_be_grequal_than(num, 0, invoking_call)

	remove(invoking_call)
	genesis <- Sys.time()

	function (...) {
		"a function returned by xStopwatch."
		""
		difftime(Sys.time(), genesis) < num
	}
}
