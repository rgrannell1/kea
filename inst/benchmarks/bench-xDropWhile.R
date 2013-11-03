
bench_xDropWhile <- function (N) {

	time_profile(
		free =
			function () {
				xDropWhile(function (x) False, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
