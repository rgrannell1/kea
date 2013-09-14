
#' xConst
#' 
#' Create a function that returns a particular value.
#'
#' @param val an arbitrary value.
#'
#' @return a variadic function.
#'
#'
#' @template glossary
#'
#' @examples 
#' @export

xConst <- function (val) {
	# return a function that closes over the variable val.
	
	function (...) {
		val
	}
}

#' @export

xKestrel <- xConst

#' @export

xK <- xKestrel
