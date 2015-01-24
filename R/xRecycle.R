
#' xRecycle
#'
#' Reuse elements of a collection in order until the collection is a certain length.
#'
#' @param
#'     num a nonnegative whole number. The length of the output collection.
#'
#' @param
#'     coll a collection. The collection to recycle elements of.
#'
#' @param
#'    ... see above.
#'
#' @return a list.
#'
#' @section Corner Cases:
#'    If \bold{coll} is empty the empty list is returned,
#'    or if \bold{num} is length-zero. If \bold{num} is shorter than the length of
#'    \bold{coll}, \bold{coll} is truncated and returned.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @template
#'    C++
#'
#' @example
#'    inst/examples/example-xRecycle.R
#'
#' @rdname xRecycle
#' @export

xRecycle <- MakeFun(function (num, coll) {

	MACRO( Must_Be_Whole(num) )

	if (length(num) == 0) {
		list()
	} else {

		MACRO( Must_Be_Between(num, 0, Inf))

		cRecycle(num, coll)

	}

})

#' @rdname xRecycle
#' @export

xRecycle_ <- MakeVariadic(xRecycle, 'coll')
