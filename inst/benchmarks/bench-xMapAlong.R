
bench_xMapAlong <- function (N) {

	time_profile(
		free =
			function () {
				xMapAlong(function (val, ind) {}, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}

