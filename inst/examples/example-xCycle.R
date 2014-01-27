
# 1. Generate all cyclic permutations of a list
#    This is really easy to do with xCycle.

all_cyclic_permutations <- coll := {
    xMap(
        ith := {
            xCycle(ith - 1, coll)
        },
        seq_along(coll)
    )
}

all_cyclic_permutations(letters[1:3])

# list(
#     list('a', 'b', 'c'),
#     list('b', 'c', 'a'),
#     list('c', 'a', 'b')
# )
