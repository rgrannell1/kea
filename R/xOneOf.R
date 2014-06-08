
#' xOneOf
#'
#' Select a random value from a collection.
#'
#' @section Type Signature:
#'     |any| -> any
#'
#' @param
#'      coll a collection. The collection to take a value from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A value from \bold{coll}.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xOneOf.R
#'
#' @family reshaping_functions
#'
#' @rdname xOneOf
#' @export

xOneOf <- MakeFun(function (coll) {

	MACRO( Fix(xOneOf, coll) )

	MACRO( Must $ Be_Collection(coll) )

	MACRO( Must $ Be_Longer_Than(0, coll) )

	if (length(coll) == 1) {
		coll[[1]]
	} else {
		# -- select a single index in a memory efficient way.
		ind <- sample.int(length(coll), 1)
		coll[[ind]]
	}
})

#' @rdname xOneOf
#' @export

xOneOf_ <- MakeVariadic(xOneOf, 'coll')
