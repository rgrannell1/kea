
bench_xLocate <- function (N) {

	time_profile(
		free =
			function () {
				xLocatel(function (x) False, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
