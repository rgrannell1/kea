
#' xIndicesTo
#'
#' Create a sequence from one to another number.
#'
#' @section Type Signature:
#'     |numeric| -> <integer>
#'
#' @param
#'     num a nonnegative number. The largest number in the sequence.
#'
#' @return
#'     an integer vector.
#'
#' @section Corner Cases:
#'     Fails when \bold{num} is infinite. If \bold{num} is zero or length-zero,
#'     \bold{integer(0)} is returned.
#'
#' @family key_functions
#'
#' @example
#'    inst/examples/example-xIndicesTo.R
#'
#' @rdname xIndicesTo
#' @export

xIndicesTo <- MakeFun(function (num) {

	MACRO( Must_Be_Orderable(num) )

	MACRO( Must_Be_Whole(num) )
	MACRO( Must_Be_Between(num, 0, Inf))

	MACRO( Must_Be_Finite(num) )

	# do not rewrite in C++
	if (num == 0 || length(num) == 0)
		integer(0)
	else
		seq_len(num)

})
