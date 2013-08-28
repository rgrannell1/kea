
#' Get the sign of a number or vector of numbers.
#'
#' @param nums a vector of doubles or integers.
#' @return returns a vector of doubles or integers.
#' @section Corner Cases:
#'	 If \code{nums} is empty, then \code{nums} is returned automatically. 
#'	 The sign of zero is zero.
#'
#' @export

#| function: xSignum version: 0.1 finished: false 

xSignum <- function (nums) {
	# Collection number -> number
	# returns the sign of a number.

	pcall <- sys.call()
	
	require_a("collection_of_length_one", nums, pcall)

	nums <- unlist(nums)

	require_a("number", nums, pcall)

	if (length(nums) == 0) {
		nums
	} else {
		sapply(nums, function (n) {
			if (n > 0) +1 else if (n == 0) 0 else -1
		})		
	}
}
