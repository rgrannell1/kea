
#
#
#
#

Try_Higher_Order_Function <- function (EXPR) {

	EXPR <- substitute(EXPR)

	bquote(tryCatch(
		.(EXPR),
		error = function (err) {

			print(str(err))
		}
	))
}
