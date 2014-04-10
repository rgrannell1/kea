
# 1. removing NA values from a collection;
#    an essential rstats task.

xReject(is.na, c(1, 2, NA, 3, NA, 4))

# list(1, 2, 3, 4)

# 2. remove numeric outliers from a data set,
#    by filtering them out.

is_outlier <- function (data_set) {
    # returns a function that tests whether an element of the
    # set is an outlier.

    data_quantile <- quantile(data_set)

    range <- list(
        lower = data_quantile[['25%']],
        higher = data_quantile[['75%']] )

    function (member) {
        member < range$lower || member > range$higher
    }
}

data_set <- c(1.0, 0.1, 1.1, 1.2, 0.1, 0.7, 0.8, 1.1)

xReject(
    is_outlier(data_set), data_set)

# list(1, 1.1, 0.8, 1.1)

# 3. HR Functional Programming #3
# Remove values larger than a certain number

# This is a job for xReject (close cousin of xSelect); it removes the values of
# a collection for which its predicate function returns true. In this case,
# we test if a number is larger than a maximum number; xReject then removes
# the values for which this is true.

elems_smaller_than <- (maximum : nums) := {
    xReject(x. > maximum, nums)
}

elems_smaller_than(10, c(1, 8, 10, 11, 100))
# c(1, 8, 10)
