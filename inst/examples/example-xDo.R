
# It is difficult to give examples for xDo, since it
# is used for side effects.

#1. print the files in your current directory.

xDo(print, list.files(.))

#    or

x_(list.files('.'))$xDo(print)

#    or even

xDo(print %of% list.files, '.')

#2. plot some data.

x_()$
xExecute(
	function () {
		plot(x = 0, y = 0, type = 'n')
	}
)$
xDo(

)