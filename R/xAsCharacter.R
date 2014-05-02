
#' xAsCharacter
#'
#' Convert a collection to a character vector.
#'
#' @section Type Signature:
#'     |character| -> <character>
#'
#' @details
#'    \code{xAsCharacter} converts a list, pairlist or vector of
#'    length-one strings to a character vector. It does not attempt
#'    to convert non-character collections to character vectors - it
#'    simply converts character lists to character typed vectors.
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
#' @family container_conversion_functions
#'
#' @example
#'    inst/examples/example-xAsCharacter.R
#'
#' @rdname xAsCharacter
#' @export

xAsCharacter <- MakeFun(function (strs) {

	MACRO( Must $ Not_Be_Missing(strs) )
	MACRO( Must $ Be_Collection(strs) )

	as_typed_vector(strs, 'character')
})

#' @rdname xAsCharacter
#' @export

xAsCharacter_ <- MakeVariadic(xAsCharacter, 'strs')
