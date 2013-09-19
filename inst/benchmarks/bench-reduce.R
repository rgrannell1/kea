
# is constructing a call and executing quicker than looping?

bench$reduce <- 
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
			reduce = 
				function (N) {

					seq <- 1:N
					new <- rep(list('+'), (2 * N) - 1)
					new[ seq(from = 2, to = length(new) + 1, by = 2) ] <- seq
					c(new, N + 1)
				}
		),
		list(
			reduce = 
				function (N) {

					seq <- 1:N
					for (ith in seq_along(seq)) {
						x <- fn( x, seq[[ith]] )
					}
				}
		)
	)