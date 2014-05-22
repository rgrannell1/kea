
# 1. generate all integer partitions of a number with distinct parts
#    The partition of a number n is a way or
#    writing n as the sum of positive integers.
#    In this case we are looking for partitions with distinct parts;
#    no number can be repeated.

# The partitions of 5 with distinct parts are
#
# 5
# 4 + 1
# 3 + 2

# These partitions are a subset of the product set
# of 1...5.

x_(1:5) $
xPowerSetOf() $
xSelect(nums := {
	(unlist %then% sum)(nums) == 5
})

# list(
#     list(2, 3),
#     list(1, 4),
#     list(5)
# )
