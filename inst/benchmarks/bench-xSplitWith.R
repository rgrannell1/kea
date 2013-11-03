
bench_xSplitWith <- function (N) {

	time_profile(
		free =
			function () {
				xSplitWith( function (x) True , seq_len(N) )
			},
		control =
			function () {
				paste0(seq_len(N))
				O_n(N)
			}
	)
}

