
# add custom highlighting js
# due to a current hack I added to staticdocs descriptions are verbatim.

sd_section(
	"Character Functions",
	paste(
		"<p>",
		"These functions are involved in text-processing, or interacting",
		"with text-files. These functions primarily involve converting text",
		"between a line, word or character representation.",
		"</p>"
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
		"xSliceString",
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
		"<p>",
		"These functions generate combinatorial structures,",
		"which are typically used for specialised patterns of iteration",
		"or generating more useful substructures from an initial pool of",
		"candidate structures. As these functions enumerate rapidly growing",
		"data structures they tend to inefficient for large inputs.",
		"</p>"
	),
	c(
		"xChoose",
		"xCycle",
		"xReorder",
		"xProdSetOf",
		"xPowerSetOf"
	)
)

sd_section(
	"Collection Comprehensions",
	paste(
		"<p>",
		"Collection comprehensions provide syntactic sugar for defining ",
		"collections by providing the contraints",
		"that each element must satisfy to be a member of the collection.",
		"These are a translation of set-builder",
		"notation, which defines a set in terms of membership constraints.",
		"</p>"
	),
	c(
		"xList"
	)
)

sd_section(
	"Container Conversion Functions",
	paste(
		"<p>",
		"These functions convert the type of container that surrounds a data set.",
		"They do not coerce the contents of the container to a different type.",
		"</p>"
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
		"<p>",
		"These functions create a subset of collection, with elements sharing a",
		"certain property. This typically involves removing null or other special values",
		"from a collection, or selecting or rejecting elements based on a predicate.",
		"</p>"
	),
	c(
		"xDropWhile",
		"xPartition",
		"xReject",
		"xSelect",
		"xTakeWhile"
	)
)

sd_section(
	"Immutable Value Functions",
	paste(
		"<p>",
		"These functions work with immutable values; references to a",
		"variable that cannot be altered by reassignment after creation.",
		"</p>"
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
		"<p>",
		"These functions are referentially impure or have side-effects",
		"like interacting with the file system.",
		"</p>"
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
		"<p>",
		"These functions involve arrow methods, or are only available as arrow methods.",
		"Most functions that are only available as methods convert second-class citizens of arrow",
		"like data frames and factors to collection representations.",
		"</p>"
	),
	c(
		"xAsDataFrame",
		"xByColkeys",
		"xByCols",
		"xByLevels",
		"xByRows",
		"xElemsByCols",
		"xExecute",
		"xElemsByRows",
		"xTap",
		"x_"
	)
)

sd_section(
	"Basic Functions",
	paste(
		"<p>",
		"These very simple functions are difficult to classify.",
		"They include the identity function, a function that ",
		"closes of a value, and functions that return true, false or na.",
		"</p>"
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
		"<p>",
		"Folds are fundemental to functional programming. A fold takes ",
		"a function that combines two values (like the ",
		"plus operator, the function 'c', or the function rbind), and applies ",
		"it to a collection of values, ultimately yielding a single value. In general, ",
		"folds can be in place of tail recursion or loops, and most functions in arrow ",
		"can be implemented as a fold.",
		"</p>"
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
		"<p>",
		"These apply a function to a data set in varying ways, or ",
		"create a new function with a different function application pattern.",
		"These include the fuction xMap and varients theiron, and functions to convert",
		"functions to different arities.",
		"</p>"
	),
	c(
		"xApply",
		"xSpread",
		"xDeepMap",
		"xFlatMap",
		"xMap",
		"xMapply",
		"xThread",
		"xUnspread",
		"xVectorise"
	)
)

sd_section(
	"Function Combining Functions",
	paste(
		"<p>",
		"These functions take several functions and combine them into",
		"a single function. These include functions for composition functions, ",
		"and for composition a function with a binary operator.",
		"</p>"
	),
	c(
		"xCompose",
		"xJuxtapose",
		"xLift"
	)
)

sd_section(
	"Function Modifying Functions",
	paste(
		"<p>",
		"These functions take a function and return a different or modified function",
		"</p>"
	),
	c(
		"xCompose",
		"xCapture",
		"xDelay",
		"xJuxtapose",
		"xLift",
		"xFix",
		"xSpread",
		"xThread",
		"xVectorise"
	)
)

sd_section(
	"Key Functions",
	paste(
		"<p>",
		"These functions deal with collection names. They modify the names of a collection, or",
		"select from a collection using keys.",
		"</p>"
	),
	c(
		"xAddKeys",
		"xPluck",
		"xUnzipIndices",
		"xUnzipKeys",
		"xZipKeys"
	)
)

sd_section(
	"Order Functions",
	paste0(
		"<p>",
		"These functions deal with the order of a collection.",
		"</p>"
	),
	c(
		"xOrderOf",
		"xRankOf"
	)
)

sd_section(
	"Parametre Functions",
	paste(
		"<p>",
		"These functions work with or alter function parametres. These include functions that",
		"return the parametres of a function, or partial application functions.",
		"</p>"
	),
	c(
		"xArityOf",
		"xIsVariadic",
		"xFormalsOf",
		"xParamsOf",
		"xFix",
		"xUnspread", "xSpread"
	)
)

sd_section(
	"Quantifier Functions",
	paste0(
		"<p>",
		"These functions count or summarise the true/false cases of a predicate",
		" over a data set.",
		"</p>"
	),
	c(
		"xAnyOf",
		"xAllOf",
		"xNoneOf",
		"xPoll"
	)
)

sd_section(
	"Reshaping Functions",
	paste(
		"<p>",
		"These functions loosely fall into the role of reshaping or adding new elements",
		"to a collection. These include functions that subdivide collections, join collections,",
		"or reorder collections.",
		"</p>"
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
		"xUnzipKeys",
		"xZipKeys",
		"xZip"
	)
)

sd_section(
	"Selection Functions",
	paste(
		"<p>",
		"These functions select or subset collections of values.",
		"This includes positional subsetting, selecting at certain",
		"keys and indices, or using a predicate for subsetting.",
		"</p>"
	),
	c(
		"xAt",
		"xAtCol",
		"xAtKey",
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
		"<p>",
		"These include functions",
		"for working with duplicated values, generating sets",
		"and the classic set operations",
		"</p>"
	),
	c(
		"xDuplicatesOf",
		"xInter",
		"xIsMember",
		"xIsSubset",
		"xNotMember",
		"xNotSubset",
		"xUnionOf",
		"xUniqueOf"
	)
)

sd_section(
	"Short-Circuiting Functions",
	paste(
		"<p>",
		"Short-circuiting functions are higher-order functions",
		"that take functions with the ability to prematurely",
		"break evaluation of a program. When Return( ) is called",
		"the result is returned immediately, improving the efficiency",
		"of the subprogram.",
		"</p>"
	),
	c(
		"xFold", "xFold",
		"xReduce", "xReduce",
		"xScan",
		"xIterate"
	)
)

sd_section(
	"Special Function Constructors",
	paste(
		"<p>",
		"These constructors are shorthands for creating functions.",
		"These included arrow functions and wildcard binary functions.",
		"</p>"
	),
	c(
		"x.",
		"xLambda"
	)
)

sd_section(
	"Time Functions",
	paste(
		"<p>",
		"These side-effectful functions work with time, or",
		"alter the temporal properties of functions. These are useful for",
		"interacting with rate-limited API's, or allowing programs to",
		"observer their own run-times.",
		"</p>"
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
		"<p>",
		"These functions test values to see if they are a particular type of value.",
		"These include functions that test for special values like null and na, and",
		"functions that test each element of a collection for these values.",
		"They are generally safer or more precise that their base counterparts.",
		"</p>"
	),
	c(
		"xIs",
		"xIsEmpty",
		"xIsFalse",
		"xIsMatch",
		"xIsNa",
		"xIsNan",
		"xIsNull",
		"xIsTrue",
		"xNot",
		"xNotEmpty",
		"xNotFalse",
		"xNotMatch",
		"xNotNan",
		"xNotNa",
		"xNotNull",
		"xNotTrue",
		"xElemIs",
		"xElemNot"
	)
)
