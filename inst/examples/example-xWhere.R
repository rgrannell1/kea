
# 1. A very trivial example.

xWhere_(True, False, True)

# c(1, 3)

# 2. Select the odd letters

x_(letters) $ x_Slice(xWhere(1:26 %% 2 == 0))

# list("b", "d", "f", "h", "j", "l", "n", "p", "r", "t", "v", "x", "z")
