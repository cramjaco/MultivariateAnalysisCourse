# https://stackoverflow.com/questions/3081066/what-techniques-exists-in-r-to-visualize-a-distance-matrix
# https://ichthyology.usm.edu/courses/multivariate/coldiss.R
"coldiss" <- function(D, nc = 4, byrank = TRUE, diag = FALSE)
{
	require(gclus)

	if (max(D)>1) D <- D/max(D)

	if (byrank) {
		spe.color = dmat.color(1-D, cm.colors(nc))
	}
	else {
		spe.color = dmat.color(1-D, byrank=FALSE, cm.colors(nc))
	}

	spe.o = order.single(1-D)
	speo.color = spe.color[spe.o,spe.o]
	
	op = par(mfrow=c(1,2), pty="s")

	if (diag) {
		plotcolors(spe.color, rlabels=attributes(D)$Labels, 
			main="Dissimilarity Matrix", 
			dlabels=attributes(D)$Labels)
		plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
			main="Ordered Dissimilarity Matrix", 
			dlabels=attributes(D)$Labels[spe.o])
	}
	else {
		plotcolors(spe.color, rlabels=attributes(D)$Labels, 
			main="Dissimilarity Matrix")
		plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
			main="Ordered Dissimilarity Matrix")
	}

	par(op)
}