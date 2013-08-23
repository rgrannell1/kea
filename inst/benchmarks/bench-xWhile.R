
bench$xWhile <- function (xs) {
	xWhile(
		function (x) x != length(xs), 
		function (x) x + 1, 1)
}
