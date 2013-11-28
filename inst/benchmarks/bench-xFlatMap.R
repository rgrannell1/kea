
bench_xFlatMap <- function (N) {

	time_profile(
		free =
			function () {
				xFlatMap(function (x) Null, seq_len(N))
			},
		control =
			function () {
				paste0(O_n(N))
			}
	)
}
