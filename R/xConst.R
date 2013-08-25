
#' @param val an arbitrary value.

xConst <- function (val) {
	# return a function that closes over the variable x.
	function (...) {
		val
	}
}

#' @export

xKestrel <- xConst

#' @export

xK <- xKestrel
