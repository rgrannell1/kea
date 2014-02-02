
# 1. xAsFunction can be an elegant way of
#    indexing a collection.

letters_fn <- xAsFunction(letters)

xMapply(
    letters_fn,
    xProdSetOf...(
        1:26, 1:26)
)
