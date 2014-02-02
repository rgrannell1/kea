
# 1. greatest common divisor:
#    a multi parametre function.

gcd <- (a : b) := {
    if (b == 0) a else gcd(b, a %% b)
}

gcd(156, 87)

# 3

