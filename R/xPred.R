
#' Get the predeccesor of a number or vector of numbers.
#'
#' @param numbers a vector of doubles or integers.
#' @return returns a vector of doubles or integers.
#' @section Corner Cases:
#' if \code{numbers} is empty, then \code{numbers} is returned automatically.
#'
#' @export

#| function: xPred version: 0.1 finished: false 

xPred <- function (numbers) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of numbers.

	pcall <- sys.call()

	require_a("listy", numbers, pcall)

	numbers <- unlist(numbers)

	require_a(c('double', 'integer'), numbers, pcall)

	if (length(numbers) == 0) {
		numbers
	} else {
		numbers - 1		
	}
}
