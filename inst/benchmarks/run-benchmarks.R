
if (Sys.info()["user"] == "ryan") {
	# avoid running on CRAN

	collated_timings <- list()

	suffix_regex <- "^bench[-][^-]+[^.]+[.][rR]"

	path <- "/home/ryan/Code/Arrow/arrow/inst/benchmarks"

	all_benchmarks <- paste0(path, "/", list.files(
		path,
		suffix_regex
	))

	source(paste0(path, "/tools-bench.R"))
	sapply(all_benchmarks, source)

	# create a linear approximation of each benchmarked
	# function, predicting the multiplier.

	time_series <- do.call(rbind, unname(Map(
		function (fn_name) {

			N_values <- 2^(1:10)
			fn <- match.fun(fn_name)

			do.call(rbind, lapply(N_values, function (N) {

				times <- fn(N)

				data.frame(
					info = rep(fn_name, length(N_values)),
					N = N,
					mean_multiplier =
						mean(times$diff$upper, times$diff$lower))
			}) )

			cat(fn_name %+% '\n')

		},
		ls(pattern = 'bench_')
	)) )

	require(ggplot2)

	ggplot(time_series) +
	geom_line(
		aes(
			x = N,
			y = mean_multiplier,
			group = info,
			colour = info,
			alpha = 1 / mean_multiplier)) +
	geom_point(
		aes(
			x = N,
			y = mean_multiplier,
			group = info,
			colour = info,
			alpha = 1 / mean_multiplier)) +
	xlab("N") + ylab("times slower than control") +
	ggtitle("runtime(profiled) / runtime(control) versus changes in N")

}

