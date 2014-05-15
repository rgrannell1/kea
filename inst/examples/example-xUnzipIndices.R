
# 1. enumerate the letters with their indices.

x_(letters) $ xUnzipIndices() $ xMapply((ith : val) := {
	xFromWords_('the ', ith, ' letter is ', val)
})
