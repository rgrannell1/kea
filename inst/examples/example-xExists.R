
# 1. Determine if a string is a cyclic permutation of another string.
#    check if there exists a way of cyclically permutting one string
#    so that the other string is identical.

is_cyclic_permutation <- (string1 : string2) := {

    is_same_length <- nchar(string1) == nchar(string2)

    string1 <- xToChars(string1)
    string2 <- xToChars(string2)

    if (length(string1) != length(string2)) {
        False
    } else {

        x_(seq_along(string1)) $
        x_Exists...(ith := {
                # does there exist a cyclic permutation such that the
                # the permutation is equal to a second string?
                identical(xCycle...(ith, string1), as.list(string2))
        })

    }

}

is_cyclic_permutation(
    'CyclicStringsForTheWin', 'ForTheWinCyclicStrings')

True
