
# the archetypal example of reduce; summing a list.

xReduce("+", 1:10)

55

# build-up a matrix from matrix rows

xReduce(
	rbind,
	list(
		c(1, 2),
		c(3, 4))
)

matrix(c(
	1, 2,
	3, 4),
	nrow = 2,
	ncol = 2,
	byrow = TRUE)
