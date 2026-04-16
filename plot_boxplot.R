# Script to generate boxplots as in Figure 16. 
# It uses estimates generated via the script estimate_fd.R.

# load the FD estimates
load('FDestimates.rda')

# generate the boxplot
pdf(file='boxplot_figure_16.pdf', width=9, height=9)


method.labels <- list(hybrid.2 = "Variogram", 
                      hybrid.1 = "Madogram", 
                      hybrid.0.5 = "Rodogram", 
                      hallwood = "Hall-Wood", 
                      periodogram = "Periodogram",
                      dctII = "DCT-II", 
                      boxcount = "Box-Count",
                      CP = "CP"
                     )
method.order <- 1:8
my.mar <- c(2.7,2.7,2,1)
my.mgp <- c(1.6,0.5,0)
par(mfrow=c(3,1), mar=my.mar, mgp=my.mgp)
for (alpha in names(fd.list)) {
	D <- 2-as.numeric(alpha)/2
	boxplot(fd.list[[alpha]][,names(method.labels)[method.order]], 
	        boxwex=0.5, range=0, whisklty='solid', xaxt='n',
			ylab='Fractal Dimension D',
			main=substitute(expression(paste(alpha==a, '  ', (D==d))), list(a=alpha, d=D))
			)
	axis(1, 1:length(method.order), labels=method.labels[method.order], las=1)
	abline(h=D, lty=2)
}
dev.off()

