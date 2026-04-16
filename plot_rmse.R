# Script to generate Figure 15. 
# It uses RMSE values generated via the script compute_rmse.R

# values of alpha
alpha <- c(0.5, 1, 1.5, 1.75)

# all methods (must be the same as the column names in the RMSE files)
methods <- c("hybrid.0.5", "hybrid.1", "hybrid.2",
            "boxcount", "periodogram", "hallwood","dctII", "CP")
# corresponding names used in the legend of the plot
methods.legend <- c("Rodogram", "Madogram", "Variogram",
                    "Box-Count", "Periodogram", "Hall-Wood", "DCT-II", "CP")
# order of the methods in the plot
plot.methods <- c(4,5,7,6,1,2,3, 8)

methods <- methods[plot.methods]
methods.legend <- methods.legend[plot.methods]

lmethods <- length(methods)

# colors
col <- c("black", rainbow(20)[c(1, 3, 7, 12, 14, 18, 20)])

# point symbols
pch <- 1:lmethods

# generate the plot
pdf(file="rmse_figure_15.pdf", width=11)
par(mfrow=c(2,2))
par(mar=c(2.5,3.8,2,1), mgp=c(1.6,0.5,0))
maxy <- 0.358
miny <- 0.001
maxx <- 65536

for (al in alpha) { # iterate over alpha
	file <- paste0("rmse_alpha=",al,".txt") # file name
	# read RMSE data
	data <- read.table(file=file, header=TRUE)
	data <- data[data[,'n']<=maxx,]
    n <- data[,'n']+1

	plot(n, data[,methods[1]], 
		main=substitute(paste(alpha==a, '  ', (D==d)), list(a=al, d=2-al/2)),
		yli=c(miny,maxy), 
		type="b", col=col[1], pch=pch[1],
		log="xy", ylab="RMSE", xaxt = "n")
	axis(1, at=c(100, 1000, 10000, 100000), labels=c('100', '1000', '10,000', '100,000'))
	for(imeth in 2:lmethods) {
    	lines(n, data[,methods[imeth]], col=col[imeth], type="b", 
						pch=pch[imeth])
	}
	legend('bottomleft', methods.legend, col=col, pch=pch, bty='n', lty=1)
}
dev.off()
