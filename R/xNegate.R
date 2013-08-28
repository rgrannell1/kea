
#' Flip the sign of a number or vector of numbers.
#'
#' @param nums a vector of doubles or integers.
#'
#' @return returns a vector of doubles or integers.
#'
#' @section Corner Cases:
#'	 if \code{nums} is empty, then \code{nums} is returned automatically.
#'
#' @export

#| function: xNegate version: 0.1 finished: false 

xNegate <- function (nums) {
	# Vector number -> number
	# returns the sign of a number, such that
	# abs x * signum x == x

	pcall <- sys.call()

	require_a("collection_of_length_one", nums, pcall)

	nums <- unlist(nums)

	require_a(c('double', 'integer'), nums, pcall)

	if (length(nums) == 0) {
		nums
	} else {
		-nums
	}
}


