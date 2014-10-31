import os



here = os.path.dirname(os.path.realpath(__file__))

def task_benchmarks():

	(_, _, filenames) = os.walk(os.path.join(here, 'R')).next()

	kea_functions  = [file for file in filenames if file.startswith('x')]

	def create_benchmark (targets):
		for target in targets:
			print(target)

	return {
		'actions':  ['create_benchmark'],
		'file_dep': [os.path.join('R', file)                  for file in kea_functions],
		'targets':  [os.path.join('inst', 'benchmarks', file) for file in kea_functions]
	}
