
bench_xCollapse <- function (N) {

	time_profile(
		free =
			function () {
				xCollapse( "", paste0(seq_len(N)) )
			},
		control =
			function () {
				paste0(O_n(N))
			}
	)
}
