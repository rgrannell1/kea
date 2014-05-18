
# 1. xThread can be used to emulate methods for the
#    base functions in R

# get the scalar square of a matrix, and sum the rows
xThread_(matrix(1:4, 2, 2),
	t,
	X := X^2,
	rowSums
)

# c(5, 25)
# is conceptually similar to

# x_(matrix(1:4, 2, 2)) $ xTap(t) $ xTap(X := X^2) $ x_Tap(rowSums)
#
# but without all the tapping.
