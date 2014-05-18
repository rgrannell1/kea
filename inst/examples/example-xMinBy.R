
# 1.
#
# Find the input that gives the smallest output for
# a given polymonial

x_(-10:10) $ xMinBy(x := {
	-x^5 - 4*x + 12*x
})

# x = 10 gives the smallest output for f(x) = -x^5 - 4x - 12x
