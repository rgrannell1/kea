
monic_polynomial_roots <- function (free, max_int) {
	# get the roots of every monic polynomial
	# with a set number of coefficients, with
	# coefficients within a range of integer magnitudes.

	as_companion <- ( function () {

		template <- matrix(0, free, free - 1)
		template[row(template)-1 == col(template)] <- 1	

		constants := {
			cbind(template, as.integer(constants))
		}
	} )()

	range <- seq(-max_int, max_int)

	x_(xSetProd)$
	xApply( xRepeat(free, list(range)) )$
	xFlatMap(
		constants := {

			x_(eigen(
				as_companion(constants),
				only.value = True)$values
			)$x()
		}
	)$x()
}


x_(monic_polynomial_roots(3, 16))$
xReject( root := {
	Im(root) == 0
} )$
xExecute(
	function () {
		par(bg = 'black')
		plot(
			x = 0, y = 0,
			xlim = c(-8, +8), ylim = c(-4, +4), 
			type = 'n')		
	}
)$
xDo(root := {
	points(
		x = Re(root), y = Im(root), 
		col = "white", cex = 0.1, 
		pch = 15)
})
