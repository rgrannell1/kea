
bench_xZipWith <- function (N) {

	time_profile(
		free =
			function () {
				xZipWith( function (n) Null, list(seq_len(N)) )
			},
		control =
			function () {
				O_n(N)
			}
	)
}
