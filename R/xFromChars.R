
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
#' @section Corner Cases:
#'    Returns the empty character-vector when \bold{strs}
#'    is length-zero.
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

xFromChars <- MakeFun('xFromChars', function (strs) {

	MACRO( Must_Not_Contain_Na(strs) )

	cImplode('', strs)
})

#' @rdname xFromChars
#' @export

xFromChars_ <- MakeVariadic(xFromChars, 'strs')
