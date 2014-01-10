
# 1. xArity works on primitive functions.

xArity('+')

2

# 2. xArity also works on normal functions
#    and variadic functions.

xArity(Reduce)

5

xArity(function (a, ...) {})

Inf

# 3. xArity can be used to check that a function
#    given to a higher-order function is safe to
#    use.

safeMap <- function (fn, coll) {

    if (!xArity(fn) == 1) {
        stop("the function given to safeMap is not unary.")
    } else {
        lapply(fn, coll)
    }
}
