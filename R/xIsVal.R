
#' xIsVal
#' 
#' Is a name binding locked?
#'
#' @param str a string or symbol.
#'
#' @return a boolean value.
#'
#' @section Corner Cases: 
#'     If a variable isn't assigned, then \code{false} is returned.
#' @template glossary
#'
#' @examples 
#' @export

xIsVal <- function (str) {
	# symbol | Vector character -> logical
	# is a name binding locked?

	pcall <- sys.call()
	pframe <- parent.frame()

	str <- match.call()$str

	assert(
		(is.character(str) && length(str) == 1) || 
		is.name(str), pcall)

	exists(toString(str), pframe) && 
		bindingIsLocked(str, pframe)
}
