
# 1. drop the last element of a toy collection.

xInitOf_(1, 2, 3)
# list(1, 2)

# 2. reverse a list recursively.

myReverse <- coll := {
	xJoin_( list(xLastOf(coll)), xReverse(xInitOf(coll)) )
}

myReverse(list(1, 2, 3, 4))

# list(4, 3, 2, 1)
