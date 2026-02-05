# Script to compute RMSE in the presence of outliers, as shown in Figure 19.

library(RandomFields)
library(fractaldim)

compute.rmse <- function(x, trueX) {
  rmse <- sqrt(mean((x-trueX)^2))
  return (rmse)
}

# values of alpha
alpha <- c(seq(0.25,1.75,by=0.25), 1.85, 1.9)

# sample size
n <- 1024

# number of replicates
repl<-1000
repl<-50

# vector of outliers
noutls <- c(0, 5)

# methods to use
methods <- list(list(name="variation", p.index=0.5), 
                list(name="variation", p.index=1), 
                list(name="variation", p.index=2),
                "periodogram", "hallwood", "dctII",
                "boxcount")
# corresponding names
method.names <- c("hybrid.0.5", "hybrid.1", "hybrid.2", 
                  "periodogram", "hallwood", "dctII",
                  "boxcount")

lmethods <- length(methods)

# On Unix-like system the RandomFieldsUtils package by default re-installs various packages.
# This is to avoid it.
RFoptions(install = "no", installPackages = FALSE)

for(noutl in noutls) { # iterate over outliers
	cat("\n# outliers = ", noutl, "\n")
	cat("====================\n")
	fdarray <- matrix(NA, ncol=repl, nrow=lmethods)

	# open file and write the header
	file <- file(paste0("rmse_", noutl, "outl.txt"), "w")
	write(c("alpha", method.names), file=file, ncolumns=1+lmethods)

	set.seed(1)
	# create a matrix of outliers for each replicate
	z <- matrix(NA, ncol=noutl, nrow=repl)
	omega <- matrix(NA, ncol=noutl, nrow=repl)
	for(iter in 1:repl) {
    	z[iter,] <- sample(1:(n+1), noutl, replace=FALSE)
		omega[iter,] <- rnorm(noutl, 0, 0.1)
	}

	for (al in alpha) { # iterate over values of alpha
  		cat("alpha = ", al, "\n")
  		trueD <- 2-al/2
  		set.seed(1)
  		error <- FALSE
		for (iter in 1:repl) { # iterate over replicates
		    # generate random field
		    ts <- try(RFsimulate(RMstable(alpha=al, var = 1, s=1), x=seq(0,1,by = 1/n), 
		                     spConform=FALSE, grid = TRUE))
         	if(inherits(ts, 'try-error')) {
             	error <- TRUE
                break
         	}
		    # add outliers
    		ts[z[iter,]] <- ts[z[iter,]] + omega[iter,]
    		
    		# estimate FD
    		fd <- fd.estimate(ts, methods=methods)
    		fdarray[,iter] <- fd$fd
  		}
  		if(!error) {
  		    # compute RMSE
  			rmse <- apply(fdarray, 1, compute.rmse, trueX=trueD)
  			write(c(al, rmse), file=file, ncolumns=1+lmethods)
  			flush(file)
  		}
  	}
	close(file)
}
print("Simulation done.")
