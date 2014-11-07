
# 1. Select a row of a data-frame list structure.

df <- xMap(x := c(x, x^2), 1:3)

x_(df) $ x_At(2)
xAt(2, df)

# list(2, 4)

# 2. Create a new positional selector

xFifthOf <- xAt(5)

# 3. Use xAt to get the values from a xGroupBy

x__('lamp', 'screen', 'cable', 'mouse') $ xGroupBy(xCharsOf) $ x_Map(xAt(2))
