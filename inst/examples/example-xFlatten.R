
# 1. flatten a tree-like collection.

tree <- list(
	list(
		list(3)
	),
	list(
		list(4),
		list(
			list(9)
		)
	)
)

xFlatten(1, tree)

# list(3, 4, 9)
