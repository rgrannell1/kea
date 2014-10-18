
#' xToChars
#'
#' Split a single string into a vector of characters.
#'
#' @section Type Signature:
#'     <character> -> <character>
#'
#' @param
#'    str a length-one character vector. The string to split into
#'    characters.
#'
#' @return
#'    A character vector of length \code{nchar(str)}, with each element being a
#'    single character.
#'
#' @section Corner Cases:
#'    Returns the empty character-vector when \bold{str}
#'    is length-zero.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xToChars.R
#'
#' @rdname xToChars
#' @export

xToChars <- MakeFun(function (str) {

	if (length(str) == 0 || nchar(str) == 0) {
		character(0)
	} else {
		str_split("", str)
	}
})
