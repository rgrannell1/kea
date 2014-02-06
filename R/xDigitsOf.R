
#' xDigitsOf
#'
#' Get the digits composing an integer.
#'
#' @param
#'      num a nonnegative whole number. The number
#'      to convert to digits.
#'
#' @return
#'    An integer vector.
#'
#' @example
#'    inst/examples/example-xDigitsOf.R
#'
#' @rdname xDigitsOf
#' @export

xDigitsOf <- function (num) {
	# number -> Vector number

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	insist $ must_be_collection(num, invoking_call)
	num <- to_value_unit(as_typed_vector(num, 'numeric'))

	insist $ must_be_of_length(num, 1)
	insist $ must_be_grequal_than(num, 0)
	insist $ must_be_whole(num, invoking_call)

	str_num <- strsplit(paste(num), '')[[1]]
	as.integer(str_num[nchar(str_num) > 0])
}
