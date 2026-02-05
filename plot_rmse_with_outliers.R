# Script to generate plot of RMSE in the presence of outliers 
# as shown in Figure 19. 
# It uses files generated via the script compute_rmse_with_outliers.R.

# methods in the input files
method.header <- c("hybrid.0.5", "hybrid.1", "hybrid.2",
             "periodogram", "hallwood", "dctII", "boxcount")
# corresponding names
methods <- c("Rodogram", "Madogram", "Variogram", 
             "Periodogram", "Hall-Wood", "DCT-II", "Box-Count")

# order of the methods shown in the plot
method.order <- c(7,4,6,5,1,2,3)

lmethods <- length(method.order)

# outliers to show
noutls <- c(0, 5)
loutl <- length(noutls)

# colors
col <- c("black", rainbow(20)[c(1, 3, 7, 12, 14, 18)])

# point symbols
pch <- 1:lmethods

pdf(file="rmse_outl_figure_19.pdf", width=13)
main <- paste(noutls, c('Outliers', 'Outliers'))

par(mfrow=c(1,loutl))

for (ioutl in 1:loutl) { # iterate over outliers
	noutl <- noutls[ioutl]
	# which file to load
	file <- c(paste0("rmse_", noutl, "outl.txt"))
	
    # read RMSE values
    data <- read.table(file=file, header=TRUE)

    alpha <- data[,"alpha"]
    data <- data[,method.header]
    lalpha <- length(alpha)

    max <- 0.17
    min <- 0.024

    plot(alpha, data[,method.order[1]], ylim=c(min,max), 
         		type="b", col=col[1], pch=pch[1], ylab="RMSE", xlab='Alpha',
         		main=main[ioutl])
    
    for (im in 2:lmethods) {    
    	lines(alpha, data[,method.order[im]], col=col[im], type="b", pch=pch[im])
    }
    legend('top', methods[method.order], col=col, pch=pch, bty='n', xjust=1, lty=1,
    	text.col=col)    
}

dev.off()
