
shortmap <- function (fn, coll) {

	if (length(coll) == 0) {
		list()
	} else {

		callCC(function (SHORT) {

			environment(fn)$SHORT <- SHORT
			lapply(coll, fn)
		})
	}
}

shortfold <- function (fn, init, coll) {

	if (length(coll) == 0) {
		init
	} else {

		callCC(function (SHORT) {

			environment(fn)$SHORT <- SHORT

			for (ith in seq_along(coll)) {
				init <- fn( init, coll[[ith]] )
			}
		})

		init
	}
}

shortfold(
	function (a, b) {
		if (b == 10) SHORT(100) else c(a, b)
	},
	list(),
	1:100
)