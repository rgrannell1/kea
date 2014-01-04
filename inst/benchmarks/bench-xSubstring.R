
bench_xSubstring <- function (N) {

	time_profile(
		free =
			function () {
				xSubstring( paste0(seq_len(N), collapse = ""), seq_len(N) )
			},
		control =
			function () {
				paste0(seq_len(N))
				O_n(N)
			}
	)
}
