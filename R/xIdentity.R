
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
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xIdentity <- function (val) {

	pcall <- sys.call()

	assert(
		!missing(val), pcall)

	val
}
#' @export

xIdiotBird <- xIdentity

#' @export

xI <- xIdiotBird
