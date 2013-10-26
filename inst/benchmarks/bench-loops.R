
# how fast are various methods of iteration?

repeat_control <- function (N) {

	ith <-1
	repeat {
		if (ith >= N) break
		ith <- ith + 1
	}
}

bench$loops <-
	xZipWith(
		function (test, control, name) {
			list(
				test = test,
				control = control,
				name = name,
				N = 10000,
				times = 5000)
		},
		list(
			"repeat" =
				function (N) {

					ith <-1
					repeat {
						if (ith >= N) break
						ith <- ith + 1
					}
				},
			"forin" =
				function (N) {
					for (i in 1:N) {

					}
				},
			"while" =
				function (N) {
					ith <- 1
					while (ith <= N) {
						ith <- ith + 1
					}
				},
			"lapply" =
				function (N) {
					lapply(1:N, function (x) Null)
				},
			"map" =
				function (N) {
					Map(function (x) Null, 1:N)
				},
			"vapply" =
				function (N) {
					vapply(1:N, function (x) 1L, integer(1))
				}
		),
		list(
			"repeat" =
				repeat_control,
			"forin" =
				repeat_control,
			"while" =
				repeat_control,
			"lapply" =
				repeat_control,
			"map" =
				repeat_control,
			"vapply" =
				repeat_control
		),
		list(
			"repeat", "forin", "while", "lapply", "map", "vapply")
	)