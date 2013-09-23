	
#' xAsVal
#' 
#' Convert a normal R variable to a constant value.
#'
#' @param str a string or symbol.
#'
#' @return Null; used for side-effect.
#'
#' @section Corner Cases: 
#'     throws an error if attempting to convert a variable that doesn't exist (in the parent frame).
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xAsVal <- function (str) {

	pcall <- sys.call()
	pframe <- parent.frame()

	assert(
		!missing(str), pcall)

	str <- toString(match.call()$str)

	assert(exists(str, envir = pframe))

	assert(
		(is.character(str) && length(str) == 1) || 
		is.name(str), pcall)

	lockBinding(str, pframe)
}
