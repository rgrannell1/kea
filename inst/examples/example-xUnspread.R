
# 1. xUnspread can be used to adapt higher-order
#    functions to make them work with non-unary functions.

xSelect_(
    xUnspread(
        (a : b) := {
            a > 0 && b >= 0
        }
    ),
    list(+1, +1),
    list(+0, +1),
    list(-1, +1),
    list(-1, -1)
)

# or as a method call

x__(list(+1, +1), list(+0, +1), list(-1, 1), list(-1, -1)) $
xSelect(xUnspread( (a : b) := {
	a > 0 && b > 0
} ))



# list(+1, +1)
