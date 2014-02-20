
progress_bar <- function (iters, invoking_call) {
	# generate a progress bar that can print
	# a summary of how long is left to run.

	fn_call <- invoking_call[[1]]
	last <- start <- as.numeric(Sys.time())

	function (iter, force = FALSE) {

		# stocastic, but faster than checking time intervals
		if (runif(1) > 0.99995 || force) {
			# emit a well-formated text progress bar.

			percent_complete <- round((iter / iters) * 100)

			seconds_elapsed <- as.numeric(Sys.time()) -  start
			seconds_estimate <- round((seconds_elapsed / iter) * iters - seconds_elapsed)

			progress_arrow <- colourise $ blue(
				gettextf('%-20s', paste0(c(rep('=', percent_complete %/% 5), '>'), collapse = '')) )

			percent_text <- gettextf('%6s', paste0(percent_complete, '%['))

			message <- paste0(
				'\r', paste0(fn_call),
				percent_text,
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
