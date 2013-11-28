
bench_xPartitionWith <- function (N) {

	time_profile(
		free =
			function () {
				xPartitionWith(function (x) True, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
