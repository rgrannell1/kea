
#' Get the successor of a number or collection of numbers.
#'
#' @param numbers a collection of doubles or integers.
#' @return returns a collection of doubles or integers.
#' @section Corner Cases:
#'     if \code{numbers} is empty, then \code{numbers} is returned automatically. If
#'     a collection 
#' @export

#| function: xSucc version: 0.1 finished: false 

xSucc <- function (numbers) {
	# Collection number -> Vector number
	# returns the successor of a vector of numbers.

	pcall <- sys.call()

	require_a("listy_of_length_one", numbers, pcall)

	numbers <- unlist(numbers)

	require_a(c('double', 'integer'), numbers, pcall)

	if (length(numbers) == 0) {
		numbers
	} else {
		numbers + 1		
	}
}
