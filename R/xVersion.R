
#' xVersion
#'
#' Get the current version number of Arrow.
#'
#' @return
#'    A three-number vector, containing the
#'    major release number, minor release number and patch number.
#'
#' @rdname xVersion
#' @export

xVersion <- function (...) {
	c(0L, 1L, 0L)
}
