colourise <- local({
	# functions that add ANSI colour codes to strings, allowing them to
	# be colourised.

	supports_colour <- function () {
		# is a terminal colourisable?

		terminal <- Sys.getenv()["TERM"]
		colour_terminals <-
			c("screen", "screen-256color", "xterm-color", "xterm-256color")

		!is.na(terminal) && (terminal %in% colour_terminals)
	}

	list(
		black =
			function (message) {
				if (supports_colour()) {
					"\033[0;30m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		blue =
			function (message) {
				if (supports_colour()) {
					"\033[0;34m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},			
		green =
			function (message) {
				if (supports_colour()) {
					"\033[0;32m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		red =
			function (message) {
				if (supports_colour()) {
					"\033[0;31m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},

		yellow =
			function (message) {
				if (supports_colour()) {
					"\033[1;33m" %+% message %+% "\033[0m"
				} else {
					message
				}
			}
	)
})
