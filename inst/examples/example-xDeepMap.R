
# 1. Recursively convert the elements of a list to strings.
#
#

tree <- list(
	1,
	list(
		2, 3
	),
	list(
		4, 5, 6, list(
			7
		)
	),
	8
)

xDeepMap(paste, tree)

# list(
#     '1',
#     list(
#         '2', '3'
#     ),
#     list(
#         '4', '5', '6', list(
#             '7'
#         )
#     ),
# '8'
# )
