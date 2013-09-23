
#' xPred
#' 
#' Increment a vector of numbers.
#'
#' @param nums a vector of non-complex numbers.
#'
#' @return a vector of non-complex numbers.
#'
#' @section Corner Cases: 
#'     returns the unit of the vector if \code{nums} is length-zero.
#'     
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xPred <- function (nums) {
	# Collection number -> Vector number
	# returns the predeccesor of a vector of nums.

	pcall <- sys.call()

	assert(
		!missing(nums), pcall)

	assert(
		is.vector(nums) || is.pairlist(nums) && is.numeric(unlist(nums)), pcall)
	
	nums <- unlist(nums)

	if (length(nums) == 0) {
		nums
	} else {
		nums - 1		
	}
}
