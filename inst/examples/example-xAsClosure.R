
# 1. The most likely use case; converting the
#    primitive operations to normal R functions.

plus <- xAsClosure('+')
plus(1, 2)

3

is.primitive(plus)

FALSE
