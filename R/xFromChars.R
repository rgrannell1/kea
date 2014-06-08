
#' xFromChars
#'
#' @section Type Signature:
#'     |character| -> &lt;character>
#'
#' Concatentate a character vector with no delimiter.
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
#' @example
#'    inst/examples/example-xFromChars.R
#'
#' @rdname xFromChars
#' @export

xFromChars <- MakeFun(function (strs) {

	MACRO( Fix(xFromChars, strs) )

	xImplode("", strs)
})

#' @rdname xFromChars
#' @export

xFromChars_ <- MakeVariadic(xFromChars, 'strs')
