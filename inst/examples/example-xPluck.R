
# 1. selecting a key from each row in a data frame.

median_lethal_dose <- list(
	'toxicant' =
		c(
			'ricin', 'dioxin', 'sarin',
			'vx', 'batrachotoxin', 'maitotoxin',
			'polonium-210', 'botox'),
	'mg_per_kg' =
		c(22, 20, 17, 2, 2, 0.13, 0.01, 0.001)
)

x_(median_lethal_dose) $ xZip() $ x_Pluck('mg_per_kg')

# list(22, 20, 17, 2, 2, 0.13, 0.01, 0.001)
