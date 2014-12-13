
#' xRank
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
#'    inst/examples/example-xRank.R
#'
#' @family math_functions
#'
#' @rdname xRank
#' @export

xRank <- MakeFun(function (nums) {

	if (length(nums) == 0)
		keep_names(integer(0), nums)
	else {

		MACRO(Must_Be_Orderable(nums))

		# -- this should be double-checked
		ranked        <- rank(nums, ties.method = 'first')

		rank_names    <- names(ranked)

		# -- dont try move this line.
		ranked        <- as.integer(ranked)
		names(ranked) <- rank_names

		ranked
	}
})

#' @rdname xRank
#' @export

xRank_ <- MakeVariadic(xRank, 'nums')
