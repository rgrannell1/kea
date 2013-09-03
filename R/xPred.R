
#' xPred
#' 
#' Increment a vector of numbers.
#'
#' @param nums a vector of non-complex numbers.
#'
#' @return a vector of non-complex numbers.
#'
#' @section Corner Cases: 
#'     returns the unit of the vector if \code{nums} is length-zero}.
#'     
#' @template glossary
#'
#' @examples 
#' @export

#| function: xPred version: 0.1 finished: false 

xPred <- function (nums) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of nums.

	pcall <- sys.call()

	require_a(c("collection_of_double", "collection_of_integer"), nums, pcall)
	nums <- unlist(nums)

	if (length(nums) == 0) {
		nums
	} else {
		nums - 1		
	}
}
