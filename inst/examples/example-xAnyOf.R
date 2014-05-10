
# 1. Using any to test if there are any true values in a vector.

xAnyOf(xI, c(True, True, False, True))

# True

# 2. Does a function name with more that 25 characters exist
#    in the base R libraries?

xAnyOf(name := nchar(name) > 25, ls('package:base'))

# several matches exist, including the terse 'suppressPackageStartupMessages'
