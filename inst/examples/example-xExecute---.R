
# 1. xExecute calls can be added to the method
#    chain to print progress messages.

LETTERS <-
	x_(letters) $
	xExecute(function () {
		cat('this part of the program is executing\n')
	}) $
	xMap(toupper) $
	x_Execute(function () {
		cat('finished!')
	})
