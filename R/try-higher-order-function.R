
#
#
#
#

Try_Higher_Order_Function <- function (EXPR) {

	EXPR <- substitute(EXPR)

	bquote(tryCatch(
		.(EXPR),
		error = function (err) {

			message <- err $ message

			throw_kiwi_error(sys.call(1), message)

		}
	))

}
