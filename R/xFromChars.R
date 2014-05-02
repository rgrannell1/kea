
#' xFromChars
#'
#' @section Type Signature:
#'     |character| -> <character>
#'
#' Concatentate a character vector with empty strs as delimiters.
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
#'    inst/examples/example-xFromChars.R
#'
#' @rdname xFromChars
#' @export

xFromChars <- function (strs) {
	xImplode("", strs)
}

#' @rdname xFromChars
#' @export

xFromChars_ <- MakeVariadic(xFromChars, 'strs')
