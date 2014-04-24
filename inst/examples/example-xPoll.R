
# 1. HR Functional Programming
#    Count the number of elements in a list without using length

# This problem is simple, but involves a slight misuse of xPoll. xPoll checks
# each element of a collection with a function and counts the number of times that
# predicate returns true. If we force every element polled to return true we
# can count the number of elements in a collection.

# xK(TRUE) returns a function that always returns TRUE. If we partially apply
# xPoll with this predicate we have a new length function.

lenof <- xPartial_(xPoll, pred = xK(TRUE))

lenof(c(1,2,3,4))
# 4
