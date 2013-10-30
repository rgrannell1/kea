
require(ggplot2)
require(reshape2)
require(simpleboot)
require(microbenchmark)

stats <- list(
	median_diff_ci =
		function (free_data, control_data) {

			# how many times slower is the free
			# data set from the control data set, within error bars.

			sampleset <- if (length(data) > 1000) {
				sample(data, size = 1000)
			} else {
				data
			}

			# what is the median distance between
			# the median of the free data, and the control data?

			boot_data <- boot.ci(
				two.boot(free_data, control_data, median, R = 2000),
				conf = 0.95, type = 'bca')

			print( boot_data$bca )
			print( median(free_data) )
			print( median(control_data) )

			# how many times faster is the test group than
			# the control group, within 95% significance bounds?

			#### FIX METHODS:
			#### not estimating mult. within error bars

			if (median(control_data) > median(free_data)) {
				list(
					lower =
						boot_data$bca[4] / median(control_data),
					upper =
						boot_data$bca[5] / median(control_data))

			} else {
				list(
					lower =
						boot_data$bca[4] / median(control_data),
					upper =
						boot_data$bca[5] / median(control_data))
			}
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

	report$difference <-
		stats$median_diff_ci(
			report$free$data, report$control$data)

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

	# each row is an observation, each col is a property
	# of that observation: lower and upper are the conf. interval bounds.
	reshaped <- as.data.frame(reshaped)
	colnames(reshaped) <- c('info', 'lower', 'upper')

	# melt the data for ggplot2 consumption.
	reshaped <- melt(
		reshaped, id.vars = 'info',
		measured = c("lower", "upper"))

	reshaped['value'] <- as.numeric( unlist(reshaped['value']) )

	ggplot(reshaped, aes(info, y = value, colour = variable)) +
	geom_bar(stat = 'identity', position = 'identity', alpha = 0) +
	xlab('') +
	ylab('times faster than control') +
	ggtitle('benchmarks')

}

test <- list(
	tprofile(
		info = "1",
		free = function () Sys.sleep(0.01),
		control = function () Sys.sleep(0.003)),
	tprofile(
		info = "2",
		free = function () Sys.sleep(0.02),
		control = function () Sys.sleep(0.015))
)

visualise_tprofile(test)
