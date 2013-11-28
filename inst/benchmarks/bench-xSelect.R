
bench_xSelect <- function (N) {

	time_profile(
		free =
			function () {
				xSelect(function (x) False, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
