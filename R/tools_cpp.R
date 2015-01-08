
error_callback <- function (message, which, frame) {
	throw_exception[[which]](sys.call(frame), message)
}

