
bench_xMapIndexed <- function (N) {

	time_profile(
		free =
			function () {
				xMapIndexed(function (val, ind) {}, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}

