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

fd.CP <- function(data, ...) {
  n <- length(data)
  diff_lag1 <- diff(data)
  CPvector <- as.numeric(diff_lag1[-1L] * diff_lag1[-(n-1L)] < 0)
  CP <- 1 / (n - 2) * sum(CPvector)
  H <- 1 + log2(sin(pi * (1 - CP) / 2))
  DCP <- 2 - H
  return(DCP)
}

estimate_fd <- function(series) {
  c(
    fractaldim::fd.estimate(series, methods=methods)$fd,
    fd.CP(series)
  )
}

lmethods <- length(methods) + 1

# corresponding names
method.names <- c("hybrid.0.5", "hybrid.1", "hybrid.2", "boxcount", 
                  "periodogram", "hallwood", "dctII", "CP")

rng.seed <- 1500

fd.list <- list()
fd <- matrix(NA, nrow=niter, ncol=lmethods)

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
    fd[iter,] <- estimate_fd(series)
  }
  colnames(fd) <- method.names
  fd.list[[as.character(alpha)]] <- fd
}
# save results
save(fd.list, file='FDestimates.rda')