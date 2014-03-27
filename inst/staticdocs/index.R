
# add custom highlighting js

sd_section(
	"Character Functions",
	paste(
		"These functions are involved in text-processing, or interacting",
		"with text-files. These functions primarily involve converting text",
		"between a line, word or character representation."
	),
	c(
		"xExplode",
		"xFromChars",
		"xFromLines",
		"xFromWords",
		"xImplode",
		"xReadChars",
		"xReadLines",
		"xReadWords",
		"xSubstring",
		"xToChars",
		"xToLines",
		"xToWords",
		"xWriteChars",
		"xWriteLines",
		"xWriteWords"
	)
)

sd_section(
	"Combinatoric Functions",
	paste(
		"These functions generate combinatorial structures,",
		"which are typically used for specialised patterns of iteration",
		"or generating more useful substructures from an initial pool of",
		"candidate structures. As these functions enumerate rapidly growing",
		"data structures they tend to inefficient for large inputs."
	),
	c(
		"xChoose",
		"xCycle",
		"xPermute",
		"xProdSetOf",
		"xPowerSetOf"
	)
)

sd_section(
	"Collection Comprehensions",
	paste(
		"Collection comprehensions provide syntactic sugar for defining ",
		"collections by providing the contraints",
		"that each element must satisfy to be a member of the collection.",
		"These are a translation of set-builder",
		"notation, which defines a set in terms of membership constraints."
	),
	c(
		"xList"
	)
)

sd_section(
	"Container Conversion Functions",
	paste(
		"These functions convert the type of container that surrounds a data set.",
		"They do not coerce the contents of the container to a different type."
	),
	c(
		"xAsCharacter",
		"xAsComplex",
		"xAsDouble",
		"xAsInteger",
		"xAsLogical",
		"xAsRaw"
	)
)

sd_section(
	"Filtering Functions",
	paste(
		"These functions create a subset of collection, with elements sharing a",
		"certain property. This typically involves removing null or other special values",
		"from a collection, or selecting or rejecting elements based on a predicate."
	),
	c(
		"xDropWhile",
		"xPartition",
		"xRejectEmpty",
		"xRejectNan",
		"xRejectNa",
		"xRejectNull",
		"xReject",
		"xSelect",
		"xTakeWhile"
	)
)

sd_section(
	"Immutable Value Functions",
	paste(
		"These functions work with immutable values; references to a",
		"variable that cannot be altered by reassignment after creation."
	),
	c(
		"xAsVal",
		"xAsVar",
		"xIsVal",
		"xVal"
	)
)

sd_section(
	"Impure Functions",
	paste(
		"These functions are referentially impure or have side-effects",
		"like interacting with the file system."
	),
	c(
		"xDelay",
		"xDo",
		"xExecute",
		"xReadChars",
		"xReadLines",
		"xReadWords",
		"xStopwatch",
		"xVersion",
		"xWriteChars",
		"xWriteWords",
		"xWriteLines"
	)
)

sd_section(
	"Methods",
	paste(
		"These functions involve arrow methods, or are only available as arrow methods.",
		"Most functions that are only available as methods convert second-class citizens of arrow",
		"like data frames and factors to collection representations."
	),
	c(
		"xAsDataFrame",
		"xByColnames",
		"xByCols",
		"xByLevels",
		"xByRows",
		"xColUnit",
		"xElemsByCols",
		"xExecute",
		"xElemsByRows",
		"xRowUnit",
		"xTap",
		"xTranspose",
		"x_"
	)
)

sd_section(
	"Basic Functions",
	paste(
		"These very simple functions are difficult to classify.",
		"They include the identity function, a function that ",
		"closes of a value, and functions that return true, false or na."
	),
	c(
		"xCapture",
		"xFalsity",
		"xIdentity",
		"xIrrelevance",
		"xTruth",
		"xUnit"
	)
)

sd_section(
	"Folding Functions",
	paste0(
		"Folds are fundemental to functional programming. A fold takes ",
		"a function that combines two values (like the ",
		"plus operator, the function 'c', or the function rbind), and applies ",
		"it to a collection of values, ultimately yielding a single value. In general, ",
		"folds can be in place of tail recursion or loops, and most functions in arrow ",
		"can be implemented as a fold."
	),
	c(
		"xFold",
		"xReduce",
		"xScan"
	)
)

sd_section(
	"Function Application Functions",
	paste(
		"These apply a function to a data set in varying ways, or ",
		"create a new function with a different function application pattern.",
		"These include the fuction xMap and varients theiron, and functions to convert",
		"functions to different arities."
	),
	c(
		"xApply",
		"xAsUnary",
		"xAsVariadic",
		"xDeepMap",
		"xFlatMap",
		"xMap",
		"xMapIndexed",
		"xMapply",
		"xThread",
		"xVectorise"
	)
)

sd_section(
	"Function Combining Functions",
	paste(
		"These functions take several functions and combine them into",
		"a single function. These include functions for composition functions, ",
		"and for composition a function with a binary operator."
	),
	c(
		"xCompose",
		"xJuxtapose",
		"xLift"
	)
)

sd_section(
	"Function Modifying Functions",
	"These functions take a function and return a different or modified function",
	c(
		"xCompose",
		"xAsClosure",
		"xAsVariadic",
		"xCapture",
		"xDelay",
		"xJuxtapose",
		"xLift",
		"xNot",
		"xPartial",
		"xThread",
		"xVectorise"
	)
)

sd_section(
	"Key Functions",
	paste(
		"These functions deal with collection names. They modify the names of a collection, or",
		"select from a collection using keys."
	),
	c(
		"xAsNamed",
		"xPluck",
		"xUnzipNames",
		"xZipNames"
	)
)

sd_section(
	"Order Functions",
	"These functions deal with the order of a collection.",
	c(
		"xOrderOf",
		"xRankOf"
	)
)

sd_section(
	"Parametre Functions",
	paste(
		"These functions work with or alter function parametres. These include functions that",
		"return the parametres of a function, or partial application functions."
	),
	c(
		"xArityOf",
		"xAsUnary", "xAsVariadic",
		"xIsVariadic",
		"xFormalsOf",
		"xParamsOf",
		"xPartial"
	)
)

sd_section(
	"Quantifier Functions",
	paste0(
		"These functions count or summarise the true/false cases of a predicate",
		" over a data set."
	),
	c(
		"xForall",
		"xExists",
		"xPoll"
	)
)

sd_section(
	"Reshaping Functions",
	paste(
		"These functions loosely fall into the role of reshaping or adding new elements",
		"to a collection. These include functions that subdivide collections, join collections,",
		"or reorder collections."
	),
	c(
		"xChop",
		"xChunk",
		"xFlatten",
		"xGroupBy",
		"xJoin",
		"xOneOf",
		"xRecycle",
		"xRepeat",
		"xReverse",
		"xShuffle",
		"xSortBy",
		"xSplitAt",
		"xSplitWith",
		"xTabulate",
		"xUnzipNames",
		"xZipNames",
		"xZip"
	)
)

sd_section(
	"Selection Functions",
	paste(
		"These functions select or subset collections of values.",
		"This includes positional subsetting, selecting at certain",
		"keys and indices, or using a predicate for subsetting."
	),
	c(
		"xAt",
		"xAtCol",
		"xDropWhile",
		"xDrop",
		"xMaxBy",
		"xMinBy",
		"xFirstOf",
		"xFourthOf",
		"xInitOf",
		"xLastOf",
		"xPluck",
		"xRestOf",
		"xSlice",
		"xSecondOf",
		"xTake",
		"xTakeWhile",
		"xThirdOf"
	)
)

sd_section(
	"Set Functions",
	paste(
		"These include functions",
		"for working with duplicated values, generating sets",
		"and the classic set operations"
	),
	c(
		"xDuplicatesOf",
		"xInter",
		"xIsMember",
		"xIsSubset",
		"xNotMember",
		"xUnionOf",
		"xUniqueOf"
	)
)

sd_section(
	"Short-Circuiting Functions",
	paste(
		"Short-circuiting functions are higher-order functions",
		"that take functions with the ability to prematurely",
		"break evaluation of a program. When Return( ) is called",
		"the result is returned immediately, improving the efficiency",
		"of the subprogram."
	),
	c(
		"xFold", "xFold",
		"xReduce", "xReduce",
		"xScan",
		"xIterate"
	)
)

sd_section(
	"Time Functions",
	paste(
		"These side-effectful functions work with time, or",
		"alter the temporal properties of functions. These are useful for",
		"interacting with rate-limited API's, or allowing programs to",
		"observer their own run-times."
	),
	c(
		"xDelay",
		"xLimit",
		"xStopwatch"
	)
)

sd_section(
	"Value Testing Functions",
	paste(
		"These functions test values to see if they are a particular type of value.",
		"These include functions that test for special values like null and na, and",
		"functions that test each element of a collection for these values."
	),
	c(
		"xIsEmpty",
		"xIsFalse",
		"xIsNa",
		"xIsNan",
		"xIsNull",
		"xIsTrue",
		"xNotEmpty",
		"xNotFalse",
		"xNotNan",
		"xNotNa",
		"xNotNull",
		"xNotTrue",
		"xElemIsFalse",
		"xElemIsNa",
		"xElemIsNan",
		"xElemIsNull",
		"xElemIsTrue",
		"xElemNotFalse",
		"xElemNotNa",
		"xElemNotNan",
		"xElemNotNull",
		"xElemNotTrue"
	)
)
