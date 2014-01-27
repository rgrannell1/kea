
# 1. HR Functional Programming
# Replaces a list of numbers with their absolute values

# This is fairly perfect use of xVectorise - a function which takes a function that
# works on one value and returns a function that works on a collection of values.
# xVectorise is really just a shorthand for partially applying xMap with a function.

absolutes <- xVectorise(abs)
absolutes(c(-1, 0, +1))

# c(1, 0, 1)

# is the same as

absolutes <- xPartial...(xMap, fn = abs)
absolutes(-1:1)

# c(1, 0, 1)
