
#' xZipKeys
#'
#' Convert a collection of name, value pairs into a named collection.
#'
#' @section Type Signature:
#'     ||any|| -> |any|
#'
#' @param
#'    colls a list or pairlist of two-element lists or pairlists,
#'    with the first element being a string and the second
#'    element being an arbitrary value. The string part of each
#'    sublist is used as the name in the output list, and the
#'    remaining part is used as the value associated with that key.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A named list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @family key_functions
#'
#' @template
#'    C++
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xZipKeys.R
#'
#' @rdname xZipKeys
#' @export

xZipKeys <- MakeFun(function (colls) {

	MACRO( Must_Be_Collection_Of_Lengths_In_Range(colls, 2, 2) )

	# -- check the key lengths too!!! TODO.

	cZipKeys(colls)
})

#' @rdname xZipKeys
#' @export

xZipKeys_ <- MakeVariadic(xZipKeys, 'colls')
