
kea ::: load_test_dependencies(environment())

message('xTranscribe')

	over(val) +

	describe("transcribing shouldn't fail") +
	worksWhen(
		True,
		xTranscribe(val),
		is.character(val) && length(val) == 1
	) +

	run()
