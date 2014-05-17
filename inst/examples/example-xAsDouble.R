
# 1. Convert a list of double values to a double vector.
#
# This also converts integers to doubles, as there is no
# information loss (all values can be coerced without failure).

xAsDouble(list(1, 2, 3, 4L, 5L))

# c(1, 2, 3, 4, 5)
