
bench$xUntil <- function (xs) {
	xUntil(
		function (x) x < length(xs), 
		function (x) x + 1, 1)
}
