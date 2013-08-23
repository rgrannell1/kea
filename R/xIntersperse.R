
#' Insert a value between every element in a collection.
#' 
#' @param x an arbitrary R value
#' @param xs a list, vector or pairlist of any length.
#'
#' @return returns a list of length n.
#' @section Corner Cases:
#'      returns \code{xs} coerced to a list if \code{xs} has has less than two elements.
#' @export

#| function: xIntersperse version: 0.1 finished: false

xIntersperse <- function (x, coll) {
	# any -> Collection any -> [any]
	# inject an element between every element
	# in coll

	pcall <- sys.call()
	require_a("any", element, pcall)
	require_a("collection", coll, pcall)

	len_coll <- length(coll)
	len_ys <- 2 * length(coll) + 1

	coll <- as.list(coll)
	
	if (length(coll) < 2) {
		coll
	} else {
		ys <- rep(list(NULL, element), len_coll)

		ys <- ys[-length(ys)]
		ith <- jth <- 1

		while (ith < len_ys) {
			ys[[ith]] <- coll[[jth]]
			jth <- jth + 1
			ith <- ith + 2
		}
		ys
	}
}
