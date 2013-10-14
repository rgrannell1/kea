
#' xSignum
#' 
#' Get the sign of a vector of numbers.
#'
#' @param nums a vector of non-complex numbers.
#'
#' @return a vector of elements in the set \code{-1, 0, +1}.
#'
#' @section Corner Cases: 
#'	 If \code{nums} is lenth-zero then the unit of that vector is returned. 
#'	 The sign of zero is zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xSignum <- function (nums) {
	# Collection number -> Vector number
	# returns the sign of a number.

	pcall <- sys.call()

	assert(
		!missing(nums), pcall,
		exclaim$parameter_missing(nums))
	
	assert(
		is_collection(nums), pcall,
		exclaim$must_be_collection(nums))

	assert(
		all(sapply(nums, length) == 1), pcall,
		exclaim$must_be_collection_of_length(nums, 1))

	nums <- coerce_to_vector(nums, 'numeric')

	if (length(nums) == 0) {
		nums
	} else {
		sapply(nums, function (num) {
			if (num > 0) +1 else if (num == 0) 0 else -1
		})		
	}
}
