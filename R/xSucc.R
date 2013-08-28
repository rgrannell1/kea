
#' Get the successor of a number or collection of numbers.
#'
#' @param nums a collection of doubles or integers.
#' @return returns a collection of doubles or integers.
#' @section Corner Cases:
#'	 if \code{nums} is empty, then \code{nums} is returned automatically. If
#'	 a collection 
#' @export

#| function: xSucc version: 0.1 finished: false 

xSucc <- function (nums) {
	# Collection number -> Vector number
	# returns the successor of a vector of nums.

	pcall <- sys.call()

	require_a("collection_of_length_one", nums, pcall)

	nums <- unlist(nums)

	require_a(c('double', 'integer'), nums, pcall)

	if (length(nums) == 0) {
		nums
	} else {
		nums + 1		
	}
}
