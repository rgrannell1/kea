
bench_xScan <- function (N) {

	time_profile(
		free =
			function () {
				xScan(function (a, b) {}, 0, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
