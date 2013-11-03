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

	cat(".")

	mb <- microbenchmark

	warmup_data <- mb(free(), times = 10)$time
	median_seconds <- median(warmup_data) / 10^9

	iters <- max(400, floor(max_time / median_seconds))

	if (iters < 20) {
		warnning ("max_time was too low to provide useful results.")
	}

	report <- list(
		info = info,
		free = list(
			data = mb(free(), times = iters)$time),
		control = list(
			data = mb(control(), times = iters)$time)
	)

	# remove the worst outliers.

	quantiles <- list(
		free = quantile(
			report$free$data, probs = c(.05, .95)),
		control = quantile(
			report$control$data, probs = c(.05, .95)) )

	report$free$data <- report$free$data[
		report$free$data > quantiles$free[[1]] &
		report$free$data < quantiles$free[[2]] ]

	report$control$data <- report$control$data[
		report$control$data > quantiles$control[[1]] &
		report$control$data < quantiles$control[[2]] ]

	# warn if the data set is too variable after kicking out outliers.

	free_sd <- sd(report$free$data) / median(report$free$data) * 100

	if (free_sd > 95) {
		message(
			info, ":timing data has an exceedingly high percentage standard-deviation (",
			round(free_sd, 0), "% of median)")
	}

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

# Big-O emulating functions.

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
