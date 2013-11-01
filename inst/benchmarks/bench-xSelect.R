
lapply_profile <- function (order) {
	function () {
		lapply(seq_len(10^order), function (x) {})
		NULL
	}
}

bench_xSelect <- list(
	info =
		"",
	free =
		( function (order) {
			function () {
				xSelect(function (x) FALSE, seq_len(10^order))
			}
		} )(order = 10),
	control =
		lapply_profile(order = 10)
)
