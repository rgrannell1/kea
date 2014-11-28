
# 1. Using any to test if there are any true values in a vector.

xAnyOf(xI, c(True, True, False, True))

# better
#
xAnyOf_(xI, True, True, False, True)

# True

# 2. Does a function name with more that 25 characters exist
#    in the base R libraries?

xAnyOf(xCharsOf %then% (x. > 25), ls('package:base'))

# several matches exist, including the terse 'suppressPackageStartupMessages'
