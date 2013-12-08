
#' xTimer
#'
#' Create a function that returns true for a certain amount of time after its creation.
#'
#' @param num a positive number.
#'
#' @return a variadic function that ignores its arguments.

#'

#' @export

xTimer <- function (num) {
	# integer -> function
	# create a timer predicate function.

	invoking_call <- sys.call()

	assert(
		!missing(num), sys.call(),
		exclaim$parameter_missing(num))

	num <- dearrowise(num)
	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) %in% 0:1, invoking_call,
		exclaim$must_have_length(num, 0:1))

	assert(
		num > 0, invoking_call,
		exclaim$must_be_greater_than(num, 0))

	genesis <- Sys.time()

	function (...) {
		difftime(Sys.time(), genesis) < num
	}
}
