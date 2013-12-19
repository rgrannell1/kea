
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xNegate')

	forall(
		"check that the kernel is properly mapped",
		list(),
		xNegate(c(-1, 0, +1)) %equals% c(+1, 0, -1)
	)

	forall(
		'the negation of the empty collection is double.',
		test_cases$collection_zero,
		xNegate(coll) %equals% double()
	)

	forall(
		'zero is an identity',
		test_cases$integers,
		xNegate(coll %% coll) %equals% as.double(coll %% coll)
	)

message('arrow $ xNegate')

	forall(
		'collection $ xNegate',
		test_cases$integers,
		x_(coll %% coll)$xNegate()$x() %equals% as.double(coll %% coll)
	)

message('arrow $ xNegate...')

	forall(
		'collection $ xNegate...',
		test_cases$integers,
		x_(coll %% coll)$xApply(xNegate...)$x() %equals% as.double(coll %% coll)
	)

message('arrow $ x_Negate')

	forall(
		'collection $ xNegate',
		test_cases$integers,
		x_(coll %% coll)$x_Negate() %equals% as.double(coll %% coll)
	)

message('arrow $ x_Negate...')

