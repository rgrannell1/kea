
bench_xReduce <- function (N) {

	time_profile(
		free =
			function () {
				xReduce(function (a, b) {}, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
