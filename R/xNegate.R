
#' Flip the sign of a number or vector of numbers.
#'
#' @param numbers a vector of doubles or integers.
#'
#' @return returns a vector of doubles or integers.
#'
#' @section Corner Cases:
#'     if \code{numbers} is empty, then \code{numbers} is returned automatically.
#'
#' @export

#| function: xNegate version: 0.1 finished: false 

xNegate <- function (numbers) {
	# Vector number -> number
	# returns the sign of a number, such that
	# abs x * signum x == x

	pcall <- sys.call()
	require_a(c('double', 'integer'), numbers, pcall)

	if (length(numbers) == 0) {
		numbers
	} else {
		-numbers
	}
}


