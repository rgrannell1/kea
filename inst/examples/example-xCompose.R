
# 1. Composing linear functions
#    composing linear function 'multiplies' them,
#    so composing x := 2x and x := 3x returns the
#    'product' function x := 6x

f <- x := 2*x
g <- x := 3*x

# h = x := 3(2*x)
# h = x := 6*x

h <- f %then% g

h(2)
# 12
