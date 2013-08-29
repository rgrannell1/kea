
#' xIdentity
#' 
#' Return an argument without modification.
#'
#' @param x an arbitrary value.
#'
#' @return returns \code{x}.
#'
#' @template glossary
#'
#' @examples 
#' @export

xIdentity <- function (x) {
	x
}

#' @export

xIdiotBird <- xIdentity

#' @export

xI <- xIdiotBird
