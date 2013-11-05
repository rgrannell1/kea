
#' xTimer
#'
#' Create a function that returns true for a certain amount of time after its creation.
#'
#' @param num a positive number.
#'
#' @return a variadic function that ignores its arguments.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xTimer <- function (num) {
	# integer -> function

	parent_call <- sys.call()

	assert(
		!missing(num), sys.call(),
		exclaim$parameter_missing(num))

	num <- dearrowise(num)
	num <- coerce_to_typed_vector(num, 'numeric', True)

	assert(
		length(num) %in% 0:1, parent_call,
		exclaim$must_have_length(num, 0:1))

	assert(
		num > 0, parent_call,
		exclaim$must_be_greater_than(num, 0))

	genesis <- Sys.time()

	function (...) {
		difftime(Sys.time(), genesis) < num
	}
}
