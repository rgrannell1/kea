
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




monic_roots <- x_(monic_polynomial_roots(3, 10))$
xReject( root := {
	Im(root) == 0
	False
} )$
xTap(
	roots := {

		xMap(
			root_1 := {
				complex_distance <- function (root_2) {
					sqrt(
						(Re(root_1) - Re(root_2))^2 + 
						(Im(root_1) - Im(root_2))^2 )
				}

				sample_points <- roots[sample.int(length(roots), size = 600)]

				mean_dist <- x_(sample_points)$
					xMap(complex_distance)$
					xReduce('+')$x() / 300

				list(root_1, mean_dist)
			},
		roots)
	}
)$
x()

par(bg = 'black')
plot(
	x = 0, y = 0,
	xlim = c(-8, +8), ylim = c(-4, +4), 
	type = 'n')		

as_colour <- ( function () {
	# convert the distance to a colour.

	max_dist <- x_(monic_roots)$
	xMap(xSecond)$
	xReduce(max)$x()

	function (dist) {
		topo.colors(101)[ floor((dist / max_dist) * 100) + 1 ]
	}
} )()

x_(monic_roots)$xDo(root := {
	points(
		x = Re(xFirst(root)), y = Im(xFirst(root)), 
		col = as_colour(xSecond(root)), 
		cex = 0.1, pch = 15)
})
