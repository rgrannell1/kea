
#' xVal
#' 
#' Assign a constant value to calling environment.
#'
#' @param str a string or symbol.
#' @param val an arbitrary value.
#'
#' @return Null; this function is used for side-effects.
#'
#' @section Corner Cases: 
#'     throws an error if a value already exists called str.
#' @template glossary
#'
#' @examples 
#' @export

xVal <- function (str, val) {
	# assign a constant value to the calling environment.

	pcall <- sys.call()
	pframe <- parent.frame()

	assert(
		!missing(str), pcall)
	assert(
		!missing(val), pcall)

	str <- toString(match.call()$str)

	assert(
		(is.character(str) && length(str) == 1) || 
		is.name(str), pcall)

	if (exists(str, envir = pframe)) {
		assert(
			!bindingIsLocked(str, pframe), pcall)
	}

	assign(str, val, envir = pframe)
	lockBinding(str, pframe)

}
