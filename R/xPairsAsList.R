
#' @param coll a list or pairlist of list or pairlist pairs, with the first element being a 
#'	string and the second element being any value.

xPairsAsList <- function (coll) {
	# Collection Collection any -> list any

	pcall <- sys.call()
	require_a(c(
		'list_of_length_two',
		'pairlist_of_length_two'),
		coll, pcall)

	if (length(coll) == 0) {
		list()
	} else {
		keys <- vapply(
			coll,
			function (elem) {
				key <- elem[[1]]
				stopifnot(is.character(key) && length(key) == 1)
				key
			},
			'string!')

		structure(
			Map( function (elem) elem[[2]], coll ),
			names = keys)		
	}
}
