
# Adapted from ggplot2's helpful messages.

.onAttach <- function (...) {

	if (!interactive() || runif(1) > 0.1) {
		return()
	}

	messages <- c(
		"Please submit bug reports and feature requests to: https://github.com/rgrannell1/kea/issues."
	)

	message <- sample(messages, 1)
	packageStartupMessage(message)
}
