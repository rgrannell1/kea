
# 1. CE Find the minimum distance
#    between any two points in a set of points.
#    xMapply could be swapped for xMap(xUnspread(fn) )
#    below, as was done with the call to reject, but
#    map is important enough to have a few variants for
#    convenience.

points <- list(
	c(12, 2),
	c(32, -4),
	c(5, -2),
	c(6, 7),
	c(9, 31)
)

# get the set product to generate all pairs of points.
x_( xProdSetOf...(points, points) ) $
# remove the pairs wich contain the same point twice.
xReject(xUnspread(identical)) $
xMapply(
	(xs : ys) := {
		# the distance function.

		sqrt( (xs[1] - xs[2])^2 + (ys[1] - ys[2])^2 )

	}
) $
x_Reduce(min)

# 7.071068
