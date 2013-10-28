
require(ggplot2)
require(reshape2)
require(simpleboot)
require(microbenchmark)

stats <- list(
	percent_mad =
		function (data) {
			100 * (mad(data) / median(data))
		},
	median_diff_ci =
		function (free_data, control_data) {
			# how many times slower is the free
			# data set from the control data set, within error bars.

			sampleset <- if (length(data) > 1000) {
				sample(data, size = 1000)
			} else {
				data
			}

			# bootstrap to find median diff, within confidence intervals.
			boot_data <- boot.ci(
				two.boot(free_data, control_data, median, R = 100000),
				conf = 0.95, type = 'bca')

			# convert to a multiplier of control data set.
			list(
				lower =
					boot_data$bca[4] / median(control_data),
				upper =
					boot_data$bca[5] / median(control_data))
		}
)

tprofile <- function (info = '', free, control, max_time = 1) {
	# profile the performance for several functions

	mb <- microbenchmark

	message('--- ', info)

	warmup_data <- mb(free(), times = 10)$time
	median_seconds <- median(warmup_data) / 10^9
	iters <- floor(max_time / median_seconds)

	if (iters == 0) {
		stop('more time needed!')
	}

	report <- list(
		info = info,
		free = list(
			data = mb(free(), times = iters)$time),
		control = list(
			data = mb(control(), times = iters)$time)
	)

	# get the median, percentage mad
	# (measure of spread) and number of iterations.

	report$free <- c(
		report$free, list(
			median =
				median(report$free$data),
			percent_mad =
				stats$percent_mad(report$free$data),
			iters = iters) )

	report$control <- c(report$control,
		list(
			median =
				median(report$control$data),
			percent_mad =
				stats$percent_mad(report$control$data),
			iters = iters) )

	# get the confidence intervals between
	# the two data sets.

	report$difference <-
		stats$median_diff_ci(
			report$free$data,
			report$control$data)

	report
}




visualise_tprofile <- function (results) {
	# visualise the results of several time
	# profiles simultaneously.

	reshaped <- Reduce(
		function (acc, new) {
			unname(rbind(acc, new))
		},
		lapply(
			results,
			function (result) {

				c(
					result$info,
					result$difference$lower,
					result$difference$upper)
		})
	)

	reshaped <- as.data.frame(reshaped)
	colnames(reshaped) <- c('profiled', 'lower', 'upper')

	reshaped <- melt(
		reshaped, id.vars = 'profiled',
		measured = c("lower", "upper"))

	reshaped['value'] <- as.numeric( unlist(reshaped['value']) )

	ggplot(reshaped, aes(profiled, y = value, fill = variable)) +
	geom_bar(stat = 'identity')

	#	ggplot(reshaped, aes(y = variable)) +
	#	geom_bar(stat = 'identity')
}

test <- list(
	tprofile(
		info = "1",
		free = function () Sys.sleep(0.1),
		control = function () Sys.sleep(0.03)),
	tprofile(
		info = "2",
		free = function () Sys.sleep(0.2),
		control = function () Sys.sleep(0.15))
)

visualise_tprofile(test)
