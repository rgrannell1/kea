
#' Get the sign of a number or vector of numbers.
#'
#' @param numbers a vector of doubles or integers.
#' @return returns a vector of doubles or integers.
#' @section Corner Cases:
#'     If \code{numbers} is empty, then \code{numbers} is returned automatically. 
#'     The sign of zero is zero.
#'
#' @export

#| function: xSignum version: 0.1 finished: false 

xSignum <- function (numbers) {
	# Collection number -> number
	# returns the sign of a number.

	pcall <- sys.call()
	
	require_a("listy", numbers, pcall)

	numbers <- unlist(numbers)

	require_a("number", numbers, pcall)

	if (length(numbers) == 0) {
		numbers
	} else {
		sapply(numbers, function (n) {
			if (n > 0) +1 else if (n == 0) 0 else -1
		})		
	}
}
