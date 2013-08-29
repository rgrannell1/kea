
#' xConst
#' 
#' Create a function that always returns the same value.
#'
#' @param x an arbitrary value.
#'
#' @return a variadic function.
#'
#'
#' @template glossary
#'
#' @examples 
#' @export

xConst <- function (x) {
	# return a function that closes over the variable x.
	function (...) {
		x
	}
}

#' @export

xKestrel <- xConst

#' @export

xK <- xKestrel
