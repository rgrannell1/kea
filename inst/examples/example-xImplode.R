
# 1. create a simple csv writer.

to_csv <- xPartial(xImplode, ', ')
to_csv(xTake(10, letters))

# c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')

