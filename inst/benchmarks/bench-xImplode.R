
bench_xImplode <- function (N) {

	time_profile(
		free =
			function () {
				xImplode( "", paste0(seq_len(N)) )
			},
		control =
			function () {
				paste0(O_n(N))
			}
	)
}
