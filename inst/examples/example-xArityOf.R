
# 1. xArityOf works on primitive functions.

xArityOf('+')

2

# 2. xArityOf also works on normal functions
#    and variadic functions.

xArityOf(Reduce)

5

xArityOf(function (a, ...) {})

Inf

# 3. xArityOf can be used to check that a function
#    given to a higher-order function is safe to
#    use.

safeMap <- function (fn, coll) {

    if (!xArityOf(fn) == 1) {
        stop("the function given to safeMap is not unary.")
    } else {
        lapply(fn, coll)
    }
}
