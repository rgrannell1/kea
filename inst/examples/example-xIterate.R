
# Example 1.
#
# find the smallest number evenly divisible by 1, 2, ..., 10
# this works a lot like a while loop that returns a value.

xIterate(
    num := {

        evenly_divisible <- num %% (1:10) == 0

        if (all(evenly_divisible)) {
            Return(num)
        } else {
            num + 1
        }
    },
    1
)
