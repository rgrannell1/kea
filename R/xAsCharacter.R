
#' xAsCharacter
#'
#' Convert a collection of characters to a character vector.
#'
#' @section Type Signature:
#'     |character| -> &lt;character>
#'
#' @details
#'    \bold{xAsCharacter} converts a list, pairlist or vector of
#'    length-one strings to a character vector. It does not attempt
#'    to convert non-character collections to character vectors - it
#'    simply converts character lists to character typed vectors.
#'
#' @param
#'    strs a collection of strings. A list, pairlist or vector
#'    of length-one character vectors to convert to a character vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A character vector.
#
#' @template
#'    Variadic
#'
#' @section Corner Cases:
#'     xAsCharacter will throw an error if its input is
#'     not a collection of characters. All length-zero collections
#'     can be converted to character.
#'
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsCharacter.R
#'
#' @template S-Uncertain
#' @rdname xAsCharacter
#' @export

xAsCharacter <- MakeFun(function (strs) {
	strs
})

#' @rdname xAsCharacter
#' @export

xAsCharacter_ <- MakeVariadic(xAsCharacter, 'strs')
