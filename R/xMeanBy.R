
#' xMeanBy
#'
#' Return the mean of a collection of numbers according to a measure function.
#'
#' @section Type Signature:
#'     (any -> <numeric>) -> |any| -> <double>
#'
#' @param
#'     fn a unary function. The function to measure the size of
#'     an element.
#'
#' @param
#'    coll a collection. The collection to find the mean of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A length-one double vector.
#'
#' @section Corner Cases:
#'    If \bold{nums} is empty then \bold{double(0)} is returned. The mean
#'    of vectors containing infinite values is infinite. The output of \bold{fn}
#'    cannot contain NA or NaN values, as these values do not have a defined
#'    magnitude.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMeanBy.R
#'
#' @family math_functions
#'
#' @rdname xMeanBy
#' @export

xMeanBy <- MakeFun(function (fn, coll) {

	if (length(coll) == 0) {
		double(0)
	} else {

		`fn(coll)` <- MACRO(Try_Higher_Order_Function(
			vapply(coll, fn, numeric(1))
		))

		# -- throw an error for unsummables
		MACRO(Must_Be_Orderable(`fn(coll)`))

		mean(`fn(coll)`)
	}
})

#' @rdname xMeanBy
#' @export

xMeanBy_ <- MakeVariadic(xMeanBy, 'coll')
