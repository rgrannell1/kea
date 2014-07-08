
#' xMeanOf
#'
#' Return the mean of a collection of numbers.
#'
#' @section Type Signature:
#'     |numeric| -> &lt;double>
#'
#' @param
#'    nums a vector of numbers. The numbers to compute the mean of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A single double vector.
#'
#' @section Corner Cases:
#'    If \bold{nums} is empty then \bold{double(0)} is returned. The mean
#'    of vectors containing infinite values is infinite. If \bold{nums}
#'    cannot contain NA or NaN values, as these values do not have a defined
#'    magnitude.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMeanOf.R
#'
#' @rdname xMeanOf
#' @export

xMeanOf <- MakeFun('xMeanOf', function (nums) {

	if (length(nums) == 0) {
		double(0)
	} else {
		# -- throw an error for unsummables
		MACRO(Must_Be_Orderable(nums))
		mean(nums)
	}
})

#' @rdname xMeanOf
#' @export

xMeanOf_ <- MakeVariadic(xMeanOf, 'nums')
