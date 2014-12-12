
error_callback <- function (message, frame) {
	throw_exception $ error(sys.call(frame), message)
}

