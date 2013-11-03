
bench_xMap <- function (N) {

	time_profile(
		free =
			function () {
				xMap(function (x) Null, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
