
#' xSignum
#' 
#' Get the sign of a vector of numbers.
#'
#' @param nums a vector of numbers.
#'
#' @return a vector of numbers.
#'
#' @section Corner Cases: 
#'	 If \code{nums} is lenth-zero then the unit of that vector is returned. 
#'	 The sign of zero is zero.
#' @template glossary
#'
#' @examples 
#' @export

#| function: xSignum version: 0.1 finished: false 

xSignum <- function (nums) {
	# Collection number -> Vector number
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
