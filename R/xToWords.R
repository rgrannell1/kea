
#' xToWords
#'
#' Split a string at whitespaces.
#'
#' @section Type Signature:
#'     &lt;character> -> &lt;character>
#'
#' @details
#'    The input string is split at spaces, tabs, and newlines,
#'    or a mix of the above. Multiple whitespaces are treated
#'    the same as one whitespace.
#'
#' @param
#'    str a length-one character vector. The string to split into
#'    whitespace-seperated words.
#'
#' @return
#'    A character vector, with one or more elements.
#'
#' @family text_processing_functions
#'
#' @example
#'    inst/examples/example-xToWords.R
#'
#' @rdname xToWords
#' @export

xToWords <- MakeFun(function (str) {

	str <- as_atom(str, 'character')

	if (nchar(str) == 0 || length(str) == 0) {
		character(0)
	} else {
		words <- strsplit(str, split = '[ \n\t]+')[[1]]
	}
})
