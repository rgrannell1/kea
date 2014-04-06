
# 1.
# Convert a function that takes a single value as an argument to
# a function that takes many arguments.
#

summate <- xAsVariadic(nums := xFold("+", 0, nums) )

summate(1, 2, 3, 4, 5, 6)

# 21
