
#' xToWords
#'
#' Split a string at whitespaces.
#'
#' @section Type Signature:
#'     <character> -> <character>
#'
#' @details
#'    The input string is split at spaces, tabs, and newlines,
#'    or a mix of the above. Multiple whitespaces are treated
#'    the same as one whitespace.
#'
#' @param
#'    str a length-one character vector. The string to split into
#'    whitespace-separated words.
#'
#' @return
#'    A character vector, with one or more elements.
#'
#' @section Corner Cases:
#'    Returns the empty character-vector when \bold{str}
#'    is length-zero.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xToWords.R
#'
#' @rdname xToWords
#' @export

xToWords <- MakeFun(function (str) {

	if (nchar(str) == 0 || length(str) == 0)
		character(0)
	else
		str_split('[ \n\t]+', str)

})
