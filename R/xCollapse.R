#' Collapse a character vector into a str with a delim.
#'
#' @param delim a length-one character vector.
#' @param strs a character vector.
#'
#' @return a str.
#'
#' @export

#| function: xCollapse version: 0.1 finished: false

xCollapse <- xAutoPartial(function (delim, strs) {
	# str -> Vector str -> str
	# Collapse a collection of strs into
	# a str with by a delim.

	pcall <- sys.call()
	require_a("string", delim, pcall)
	require_a("character", strs, pcall)

	paste0(strs, collapse = delim)

})
