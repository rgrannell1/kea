
#' xIdentity
#' 
#' Return an argument without modification.
#'
#' @param val an arbitrary value.
#'
#' @return returns \code{val}.
#'
#' @template glossary
#'
#' @examples 
#' @export

xIdentity <- function (val) {
	val
}
#' @export

xIdiotBird <- xIdentity

#' @export

xI <- xIdiotBird
