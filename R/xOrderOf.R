
#' xOrderOf
#'
#' Return a permutation of indices that reorders a collection of numbers.
#'
#' @section Type Signature:
#'     |numeric| -> &lt;integer>
#'
#' @details
#'    \bold{xOrderOf} returns the indices for a collection that are required to
#'    re-order it. For example,
#'
#'    \code{c(3, 1, 2)[ xOrderOf(c(3,1,2)) ]}
#'
#'    re-arranges the collection as
#'
#'    \code{c(1, 2, 3)}
#'
#'    This is a trivial use of \bold{xOrderOf}; a more common use is to
#'    re-order a collection of collections by the indices of one row or column.
#'
#' @param
#'    nums a vector of numbers. The numbers to rank
#'    in order of size.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of numbers.
#'
#' @section Corner Cases:
#'    If nums is empty then \bold{integer(0)} is returned. Nums cannot contain
#'    na or nan values, as then are unorderable.
#'
#' @template
#'    Variadic
#'
#' @family math_functions
#'
#' @example
#'    inst/examples/example-xOrderOf.R
#'
#' @rdname xOrderOf
#' @export

xOrderOf <- MakeFun('xOrderOf', function (nums) {


	if (length(nums) == 0) {
		keep_names(integer(0), nums)
	} else {

		# -- throw an error for unorderables in nums;
		# -- NaN, Na.

		MACRO(Must_Be_Orderable(nums))
		order(nums)
	}
})

#' @rdname xOrderOf
#' @export

xOrderOf_ <- MakeVariadic(xOrderOf, 'nums')
