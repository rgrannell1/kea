
#' xRankOf
#'
#' Rank a collection of numbers from largest to smallest.
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
#'    If nums is empty then \bold{integer(0)} is returned. Tied values are
#'    ranked in order of appearance; the first occurrence is ranked lower than
#'    the second occurrence. Na or NaN values cannot be included, as they are
#'    unorderable.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xRankOf.R
#'
#' @rdname xRankOf
#' @export

xRankOf <- MakeFun('xRankOf', function (nums) {

	if (length(nums) == 0) {
		keep_names(integer(0), nums)
	} else {

		MACRO(Must_Be_Orderable(nums))

		# -- this should be double-checked
		as.integer(rank(nums, ties.method = 'first'))
	}
})

#' @rdname xRankOf
#' @export

xRankOf_ <- MakeVariadic(xRankOf, 'nums')
