
# 1. I'm not aware of any legitimate uses for the non-variadic form of xInvoke;
#    it is basically the same as calling a function directly.

xInvoke(x. ^ 2, 2)

# 4

# BAD

xMap(xInvoke(x. ^ 2), 1:10)

# BETTER; equivalent, but faster.

xMap(x. ^ 2, 1:10)

# 2. xInvoke_ is much more useful; it turns non-variadic functions that take a list into
#    variadic functions

myLen <- coll := length(coll)

myVariadicLen <- xInvoke_(myLen)

# -- now you can call the length function with a variable number of arguments.

myVariadicLen(1, 2, 3, 4, 5)
