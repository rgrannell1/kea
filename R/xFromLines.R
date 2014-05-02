
#' xFromLines
#'
#' Concatentate a character vector with newlines as delimiters.
#'
#'
#' @section Type Signature:
#'     |character| -> <character>
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
#'    inst/examples/example-xFromLines.R
#'
#' @rdname xFromLines
#' @export

xFromLines <- function (strs) {
	xImplode("\n", strs)
}

#' @rdname xFromLines
#' @export

xFromLines_ <- MakeVariadic(xFromLines, 'strs')
