
bench_xSubStr <- function (N) {

	time_profile(
		free =
			function () {
				xSubStr( paste0(seq_len(N), collapse = ""), seq_len(N) )
			},
		control =
			function () {
				paste0(seq_len(N))
				O_n(N)
			}
	)
}
