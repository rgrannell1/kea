require(ggplot2)
require(reshape2)
require(microbenchmark)

time_profile <- function (info = '', free, control, max_time = 1) {
	# profile the performance for several functions

	multiplier <- function (free_data, control_data) {

		# how many times slower is the free
		# data set from the control data set, within error bars.

		free_report <- unname(quantile(free_data))
		control_report <- unname(quantile(control_data))

		list(
			info =
				"",
			lower =
				free_report[2] / control_report[4],
			upper =
				free_report[4] / control_report[2])
	}

	mb <- microbenchmark

	message('--- ', info)

	warmup_data <- mb(free(), times = 10)$time
	median_seconds <- median(warmup_data) / 10^9
	iters <- floor(max_time / median_seconds)

	if (iters == 0) {
		stop("max_time was too low to allow for a single iteration.")
	}

	report <- list(
		info = info,
		free = list(
			data = mb(free(), times = iters)$time),
		control = list(
			data = mb(control(), times = iters)$time)
	)

	# get the median, and number of iterations.

	report$free <- c(
		report$free, list(
			median =
				median(report$free$data),
			iters = iters) )

	report$control <- c(report$control,
		list(
			median =
				median(report$control$data),
			iters = iters) )

	# get the confidence intervals between the two data sets.

	report$diff <-
		multiplier(
			report$free$data, report$control$data)

	report
}


O_n <- function (N) {
	lapply(seq_len(N), function (x) {})
}

#visualise_time_profile(
#	list(time_profile(
#		"",
#		function () Sys.sleep(0.01) ,
#		function () Sys.sleep(0.02)
#	))
#)
