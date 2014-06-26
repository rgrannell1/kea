
#' xFromLines
#'
#' Concatentate a character vector with newlines as delimiters.
#'
#' @section Type Signature:
#'     |character| -> &lt;character>
#'
#' @param
#'    strs a collection of strings. The
#'    strings to collapse into one string.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A length-one character vector.
#'
#' @section Corner Cases:
#'    Returns the empty character-vector when \bold{strs}
#'    is length-zero.
#'
#' @family text_processing_functions
#'
#' @template
#'    Variadic
#'
#'
#' @example
#'    inst/examples/example-xFromLines.R
#'
#' @rdname xFromLines
#' @export

xFromLines <- MakeFun('xFromLines', function (strs) {

	MACRO( Must_Not_Contain_Na(strs) )

	if (length(strs) == 0) {
		character(0)
	} else {
		paste(strs, collapse = '\n')
	}
})

#' @rdname xFromLines
#' @export

xFromLines_ <- MakeVariadic(xFromLines, 'strs')
