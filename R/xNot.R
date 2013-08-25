
#' Return the logical negation of a function.

#' @param pred a logical function.

#' @export

xNot <- function (pred) {
	# function -> function
	
	xCompose(function (x) !x, pred)
}
