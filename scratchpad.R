
rPaths <- list.files("/home/ryan/packages", '.[R]$', full.names = True, recursive = True)


allFunctions <- expr := {

	allVariables    <- all.names(expr)
	allNonFunctions <- all.vars(expr, unique = False)

	allVariables[!(allVariables %in% allNonFunctions)]
}

stringifyTable <- table := {

	'%+%' <- (a : b) := paste0(a, b)

	inner <- x_(table) $ xMap(pair := {

		"list(" %+%
			"`" %+% toString(xFirstOf(pair)) %+% "`" %+% "," %+%
			toString(xSecondOf(pair)) %+%
		")"

	}) $ x_Implode(",")

	"list(" %+% inner %+% ")"
}

mergeTable <- (left : right) := {

	leftKeys  <- xAtCol(1, left)
	rightKeys <- xAtCol(1, right)

	keys <- xUniqueOf(c(leftKeys, rightKeys))

	x_(keys) $ x_Map(key := {

		isLeftMember  <- xIsTrue(xIsMember(key, leftKeys))
		isRightMember <- xIsTrue(xIsMember(key, rightKeys))

		newsum <-
		if (isLeftMember && isRightMember) {
			0
		} else if (isLeftMember) {
			left[[key]]
		} else {
			left[[key]] + right[[key]]
		}

		list(key, newsum)
	})
}

packageFunctions <- x_(rPaths) $ xTake(10) $ xMap(path := {

	script <- tryCatch(
		parse(path),
		error = xK(Null),
		warning = xK(Null)
	)

	xTabulate(allFunctions(script))
}) $
xReduce(mergeTable)
