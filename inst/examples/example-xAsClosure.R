
# 1. The most likely use case; converting the
#    primitive operations to normal R functions.

plus <- xAsClosure('+')
plus(1, 2)

# 3

is.primitive(plus)

# FALSE

# 2. Arrow functions are usually fine with primitive functions.
#

x_(1:3) $ x_Scan(plus, 0)

# list(0, 1, 3, 6)

x_(1:3) $ x_Scan('+', 0)

# list(0, 1, 3, 6)
