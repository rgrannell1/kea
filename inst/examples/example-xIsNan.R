
# 1. Division by zero generates NaN,
# and the value may pop up (often erroneously) in other contexts.

val <- 0 / 0

xIsNan(val)

# True
