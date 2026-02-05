# Script to compute RMSE in Figure 15
# Results are stored in ASCII files, one per value of alpha

library(RandomFields)
library(fractaldim)

compute.rmse <- function(x, trueX) {
  mse <- mean((x-trueX)^2)
  return (sqrt(mse))
}

# values of alpha
alpha <- c(0.5, 1, 1.5, 1.75)

# sample sizes
n <- c(32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536#, 131072
			) # using all values will take long time to run
#n<-c(32, 64, 128, 256) # for testing

# number of replicates
repl<-1000

# which methods to use
methods <- list(list(name="variation", p.index=0.5), 
                list(name="variation", p.index=1), 
                list(name="variation", p.index=2),
                "boxcount", "periodogram", "hallwood", "dctII"
                )
method.names <- c("hybrid.0.5", "hybrid.1", "hybrid.2", "boxcount", 
                  "periodogram", "hallwood", "dctII"
                  )
lmethods <- length(methods)

# init array for the FD estimates
fdarray <- matrix(NA, ncol=repl, nrow=lmethods)

# On Unix-like system the RandomFieldsUtils package by default re-installs various packages.
# This is to avoid it.
RFoptions(install = "no", installPackages = FALSE)

for (al in alpha) { # loop over values of alpha
  cat("alpha = ", al, "\n")
  trueD <- 2-al/2
  # open a file to store results and write the header
  file <- file(paste0("rmse_alpha=",al,".txt"), "w") 
  write(c("n", method.names), file=file, ncolumns=1+lmethods) 
  for (nn in n) { # iterate over sample sizes
    cat("n = ", nn, "\n")
    set.seed(1)
    error <- FALSE
    for (iter in 1:repl) {
      # simulate random fields
      ts <-   try(RFsimulate(RMstable(alpha=al, var = 1, s=1), x=seq(0,1,by = 1/nn), 
                           spConform=FALSE, grid = TRUE)
                  )
      if(inherits(ts, 'try-error')) {
       	error <- TRUE
        break
      }
      # estimate for methods
      fd <- fd.estimate(ts, methods=methods)
      
      # save results
      fdarray[,iter] <- fd$fd
    }
    # compute RMSE and store into file
    if(!error) {
    	rmse <- apply(fdarray, 1, compute.rmse, trueX=trueD)
    	write(c(nn, rmse), file=file, ncolumns=1+lmethods)
    	flush(file)
    }
  }
  close(file)
}
print("Simulation done.")
