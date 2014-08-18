
# 1. a toy use

xFromChars(xRiffle_(',', 'a', 'b', 'c'))

# "a,b,c"
#
# a more idiomatic way to achieve this would be with
#
# xImplode_(',', 'a', 'b', 'c')

# 2. inject elements into a list.
#
# by combining xRiffle with xJoin you can inject lists between
# each element of a collection. The following example isn't very
# practicaly, but demonstrates the complex lists you can build with
# xRiffle well.

x_(paste(1:5)) $ xRiffle(list('+', '10', '*')) $ xJoin() $ x_FromWords()

# "1 + 10 * 2 + 10 * 3 + 10 * 4 + 10 * 5"
