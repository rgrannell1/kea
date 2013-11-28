
bench_xShuffle <- function (N) {

	time_profile(
		free =
			function () {
				xShuffle(seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
