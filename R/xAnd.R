
#' Return a function that tests if two functions are true for a particular value.

#' @param pred1 a logical function.
#' @param pred2 a logical function.

#' @export

xAnd <- xAutoPartial(function (pred1, pred2) {
	# (a -> logical) -> (a -> logical) -> (a -> logical)
	xPhoenix("&&", pred1, pred2)
})
