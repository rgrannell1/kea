
error_callback <- function (message) {
	throw_kea_error(sys.call(1), message)
}
