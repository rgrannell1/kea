
# 1. Select a row of a data-frame list structure.

df <- xList[c(a, a^2), a <- 1:3]

x_(df) $ x_At(2)

# list(2, 4)

# 2. Create a new positional selector

xFifthOf <- xPartial(xAt, 5)
