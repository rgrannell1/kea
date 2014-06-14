
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
				left[[key]] + right[[key]]
			} else if (isLeftMember) {
				left[[key]]
			} else if (isRightMember) {
				right[[key]]
			} else {

				0
			}

		list(key, newsum)
	})
}

packageFunctions <- x_(rPaths) $ xTake(3) $ xFold((longest : path) := {

	script <- tryCatch(
		parse(path),
		error = xK(Null),
		warning = xK(Null)
	)

	func <- allFunctions(script)

	if (length(func) > 0) {
		this_longest <- func[[ which.max(nchar(func)) ]]

		if (nchar(this_longest) > nchar(longest)) this_longest else longest

	} else {
		longest
	}


}, "")





uniqueFunctions <- x_(rPaths) $ xTake(Inf) $ xMap(path := {

	script <- tryCatch(
		parse(path),
		error = xK(Null),
		warning = xK(Null)
	)

	xUniqueOf(allFunctions(script))
}) $
xFlatten(1) $
xUniqueOf()





packageFunctions <- x_(rPaths) $ xTake(3) $ xMap(path := {

	script <- tryCatch(
		parse(path),
		error = xK(Null),
		warning = xK(Null)
	)

	xTabulate(allFunctions(script))
}) $
xReduce(mergeTable)

















a1 <- data.frame(a = c(aa = 1, bb = 2), b = c(aa = 2, bb = 3))
a2 <- data.frame(a = c(aa = 2, bb = 3), d = c(ee = 3, ff = 4))

# merge(a1, a2)

#     x, y: data frames, or objects to be coerced to one.

# by, by.x, by.y: specifications of the columns used for merging.  See
#           ‘Details’.

#      all: logical; ‘all = L’ is shorthand for ‘all.x = L’ and ‘all.y =
#           L’, where ‘L’ is either ‘TRUE’ or ‘FALSE’.

#    all.x: logical; if ‘TRUE’, then extra rows will be added to the
#           output, one for each row in ‘x’ that has no matching row in
#           ‘y’.  These rows will have ‘NA’s in those columns that are
#           usually filled with values from ‘y’.  The default is ‘FALSE’,
#           so that only rows with data from both ‘x’ and ‘y’ are
#           included in the output.

#    all.y: logical; analogous to ‘all.x’.

#     sort: logical.  Should the result be sorted on the ‘by’ columns?

# suffixes: a character vector of length 2 specifying the suffixes to be
#           used for making unique the names of columns in the result
#           which not used for merging (appearing in ‘by’ etc).

# incomparables: values which cannot be matched.  See ‘match’.

#     ...: arguments to be passed to or from methods.





x_(list.files('/home/ryan/Code/kiwi.R', full.names = TRUE, recursive = TRUE)) $
xReject(path := {
	xIsMatch('DESCRIPTION|png', path)
}) $
xMap(xReadWords) $ xFlatten(1) $
xSelect(word := {
	xIsMatch('_', word)
}) $
xUniqueOf() $
xReject(word := {

})




methods <-
	x_(xReadLines('/home/ryan/Code/kiwi.R/NAMESPACE')) $
	xSelect(line := {
		xIsMatch('export', line)
	}) $
	xMap(line := {
		xExplode('export[(]|[)]', line)
	}) $
	xReject(line := {
		xIsMatch('_', line)
	})

methods $ xSelect(fn := xIsMatch('Is', fn))
methods $ xSelect(fn := xIsMatch('Not', fn))












freq <- base_or_utils <-
    x_(ls('package:base')) $
    x_Join_(ls('package:utils'))

inner_calls $ xAtCol(3) $ xFlatten(1) $ xSelect(name := {
    xIsMember(name, base_or_utils)
}) $ xTabulate() $ xSortBy(xSecondOf)








fn <- (a_: b_: c_) := {
    a_ $ xMap(toupper)
}
