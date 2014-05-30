
# 1. Test which elements in a collection are NaN.

not_nan_element <- x_(c(1, 2, 3, 4, NaN, 5, 6, NaN)) $ x_ElemNot(NaN)

# c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
