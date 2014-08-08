
# 1. Subsetting a collection
#
# Slice is the replacement for '[[' in Kea. It behaves similarly.

nums <- 1:10

x_(nums) $ x_Slice(xWhere(nums %% 3 == 0))

# list(3, 6, 9)
#
# which is similar to

# nums[w nums %% 3 == 0 ]

# using standard subsetting.
