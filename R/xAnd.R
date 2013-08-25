
#' Return a function that tests if two functions are true for a particular value.

#' @param pred_1 a logical function.
#' @param pred_2 a logical function.

#' @export

xAnd <- function (pred_1, pred_2) {
	# (a -> logical) -> (a -> logical) -> (a -> logical)
	xPhoenix("&&", pred_1, pred_2)
}
