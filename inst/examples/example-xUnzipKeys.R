
# 1. xUnzipkeys can be useful for
#    reformatting a named list before passing it
#    to a function like map or select, allowing those
#    functions access to both the key and value.

irish_birds <- list(
	'Grey Heron'  = 'Ardeidae',
	'Black Stork' = 'Ciconiiformes',
	'Giant Ibis'  = 'Ciconiiformes'
)

x_(irish_birds) $ xUnzipKeys() $ x_Map(xUnspread((key : val) := {

		xFromWords_('the', key, 'is of order', val)

	})
)

# list(
#     "the  Grey Heron is of order Ardeidae",
#     "the  Black Stork is of order Ciconiiformes",
#     "the  Giant Ibis is of order Ciconiiformes")
