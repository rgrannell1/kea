
# 1. Replace NaN and NA values with 0.

xSwap( NaN, 0, xSwap(Na, 0, c(NaN, 1, 2, 3, 4, Na, -10)) )

# or the much more readable.
#
x__(NaN, 1, 2, 3, 4, Na, -10) $ xSwap(NaN, 0) $ x_Swap(Na, 0)

# list(0, 1, 2, 3, 4, 0, -10)
