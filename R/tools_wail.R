
# -------------------------------- wail -------------------------------- #
#
# To Developers,
#
# wail is specific to customised errors thrown by the arrow method chaining unit tests,
# particularily the more complicated ones.
#
# It is named wail after the noise I emit when a unit test fails_

wail <- list(
	normal_form_missing =
		function (method, proto, actual) {

			'the xMethod form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'
		},
	unchaining_form_missing =
		function (method, proto, actual) {

			'the x_Method form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'

		},
	variadic_form_missing =
		function (method, proto, actual) {
			'the xMethod_ form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'

		},
	variadic_unchaining_form_missing =
		function (method, proto, actual) {
			'the x_Method_ form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'

		},
	method_not_in_proto =
		function (method, proto) {
			'the method ' %+% method %+% ' should be in the prototype ' %+% proto %+%
			' but was not.'

		},
	unchaining_calls_x_ =
		function (method) {
			'the method ' %+% method %+%
			' was supposed to be unchaining but called x_'

		},
	chaining_must_call_x_ =
		function (method) {
			'the method ' %+% method %+%
			' was supposed to be chaining but didnt call x_'

		},
	variadic_must_call_... =
		function (method) {

			'the method ' %+% method %+%
			' was supposed to be variadic but didnt call ...'

		},
	non_variadic_calls_... =
		function (method) {

			'the method ' %+% method %+%
			' was supposed to be non-variadic but called ...'

		},
	function_not_in_method =
		function (method) {

			'the method ' %+% method %+%
			' should have called its underlying function.'
		}
)
