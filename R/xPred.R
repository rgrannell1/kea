
#' Get the predeccesor of a number or vector of numbers.
#'
#' @param nums a vector of doubles or integers.
#' @return returns a vector of doubles or integers.
#' @section Corner Cases:
#' if \code{nums} is empty, then \code{nums} is returned automatically.
#'
#' @export

#| function: xPred version: 0.1 finished: false 

xPred <- function (nums) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of nums.

	pcall <- sys.call()

	require_a("collection_of_length_one", nums, pcall)

	nums <- unlist(nums)

	require_a(c('double', 'integer'), nums, pcall)

	if (length(nums) == 0) {
		nums
	} else {
		nums - 1		
	}
}
