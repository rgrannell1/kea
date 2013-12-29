
bench_xPartition <- function (N) {

	time_profile(
		free =
			function () {
				xPartition(function (x) True, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
