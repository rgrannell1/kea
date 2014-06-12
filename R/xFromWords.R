
#' xFromWords
#'
#' Concatentate a character vector with spaces as delimiters.
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
#' @family text_processing_functions
#'
#' @template
#'    Variadic
#'
#'
#' @example
#'    inst/examples/example-xFromWords.R
#'
#' @rdname xFromWords
#' @export

xFromWords <- MakeFun(function (strs) {

	strs <- as_typed_vector(strs, 'character')

	if (length(strs) == 0) {
		character()
	} else {
		paste(strs, collapse = ' ')
	}
})

#' @rdname xFromWords
#' @export

xFromWords_ <- MakeVariadic(xFromWords, 'strs')
