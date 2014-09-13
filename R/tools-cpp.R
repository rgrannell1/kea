
error_callback <- function (message, frame) {
	throw_kea_error(sys.call(frame), message)
}
