
#' xTimer
#' 
#' Create a function that returns true for a certain amount of time after its creation.
#'
#' @param num a positive number.
#'
#' @return a variadic function that ignores its arguments.
#'
#' @template glossary
#'
#' @examples 
#' @export

xTimer <- function (num) {
	# integer -> function

	genesis <- Sys.time()
	function (...) {
		difftime(Sys.time(), genesis) < num
	}
}
