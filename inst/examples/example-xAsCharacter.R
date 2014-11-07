
# 1. Convert a list of character values to a character vector.

xAsCharacter(list('this', 'is', 'much', 'words'))

# c('this', 'is', 'much', 'words')

# 2. This does NOT work

# xAsCharacter(1L)
#
# xAsCharacter only unlists character vectors, it doesn't convert
# numbers to strings.
