
progress_bar <- function (iters) {
	# generate a progress bar that can print
	# a summary of how long is left to run.

	last <- start <- as.numeric(Sys.time())

	function (iter) {

		if (as.numeric(Sys.time()) - last > 0.5) {
			# emit a well-formated text progress bar.

			percent_complete <- round((iter / iters) * 100)

			seconds_elapsed <- as.numeric(Sys.time()) -  start
			seconds_estimate <- round((seconds_elapsed / iter) * iters - seconds_elapsed)

			progress_arrow <- colourise $ blue(
				gettextf('%-20s', paste0(c(rep('=', percent_complete %/% 5), '>'), collapse = '')) )

			message <- paste0(
				'\r', percent_complete, '%[',
				progress_arrow,
				'] ',
				iter, ',', iters,
				gettextf("%8s", seconds_estimate), 's'
			)

			if (iter == iters) {
				cat(message, '\n\n')
			} else {
				cat(message)
			}

			last <<- as.numeric(Sys.time())
		}
	}
}
