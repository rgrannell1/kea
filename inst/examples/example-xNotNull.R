
# 1. An obvious example.

xNotNull(Null)

# False

# 2. filter out null values.

x__(1, 2, NULL, 3) $ xSelect(xNotNull)

# list(1, 2, 3)
