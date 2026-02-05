# Script to compute FD estimates shown in Figure 16.

library(RandomFields)
library(fractaldim)

# sample size
n <- 1024

# values of alpha
alphas <- c(0.4, 1, 1.6)
niter <- 500

# methods to use
methods <- list(list(name="variation", p.index=0.5), 
                list(name="variation", p.index=1), 
                list(name="variation", p.index=2),
                "boxcount", "periodogram", "hallwood", "dctII")
# corresponding names
method.names <- c("hybrid.0.5", "hybrid.1", "hybrid.2", "boxcount", 
                  "periodogram", "hallwood", "dctII")
rng.seed <- 1500

fd.list <- list()
fd <- matrix(NA, nrow=niter, ncol=length(methods))

# On Unix-like system the RandomFieldsUtils package by default re-installs various packages.
# This is to avoid it.
RFoptions(install = "no", installPackages = FALSE)

for (alpha in alphas) { # iterate over alpha
	set.seed(rng.seed)
	for(iter in 1:niter) {
	    # simulate random field
	    series <- RFsimulate(RMstable(alpha=alpha, var = 1, s=1), x=seq(0,1,by=1/n), 
	                       spConform=FALSE, grid = TRUE)
	    # estimate
		fd[iter,] <- fd.estimate(series, methods=methods)$fd
	}
	colnames(fd) <- method.names
	fd.list[[as.character(alpha)]] <- fd
}
# save results
save(fd.list, file='FDestimates.rda')
