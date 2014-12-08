
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

# x_(matrix(1:4, 2, 2)) $ xInvoke(t) $ xInvoke(X := X^2) $ x_Invoke(rowSums)
#
# but without all the boilerplate.
