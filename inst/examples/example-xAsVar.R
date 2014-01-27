
# 1. xAsVar can be used to unlock values back to
#    variables, since every function needs an inverse.

const <- list(
    letters =
        letters
)

xAsVal(const)
xAsVar(const)

const <- NULL
const

# NULL
