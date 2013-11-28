
bench_xFoldList <- function (N) {

	time_profile(
		free =
			function () {
				xFoldList(function (a, b) {}, 0, seq_len(N))
			},
		control =
			function () {
				O_n(N)
			}
	)
}
