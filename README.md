This repository contains code to reproduce results from the paper [**Estimators of fractal dimension: Assessing the roughness of time series and spatial data.** (2012) by Tilmann Gneiting, Hana Sevcikova and Don B. Percival, published in Statistical Science, 27(2), 247-277.](https://projecteuclid.org/journals/statistical-science/volume-27/issue-2/Estimators-of-Fractal-Dimension--Assessing-the-Roughness-of-Time/10.1214/11-STS370.full)

To code makes an extensive use of the RandomFields R package which has been archived since the publication. However, it is still functioning and available on CRAN archive or on the author's [GitHub repo](https://github.com/schlather/PACKAGES/tree/main). The code in this repo was tested with the CRAN archive versions of the packages, namely by installing the source versions of [RandomFieldsUtils](https://cran.r-project.org/src/contrib/Archive/RandomFieldsUtils/) (1.2.5) and [RandomFields](https://cran.r-project.org/src/contrib/Archive/RandomFields/) (3.3)

Furthermore, install the [fractaldim](https://cran.r-project.org/web/packages/fractaldim/index.html) package from CRAN.

To reproduce Figure 15, first run the script [compute_rmse.R](compute_rmse.R) which generates ASCII files containing RMSE values (one file for each value of alpha), then run the script [plot_rmse.R](plot_rmse.R) to generate the plot.

To reproduce Figure 16, first run the script [estimate_fd.R](estimate_fd.R) to generate a distribution of FD estimates (stored in a binary file), then run the script [plot_boxplot.R](plot_boxplot.R) to generate the boxplot.

To reproduce Figure 19, first run the script [compute_rmse_with_outliers.R](compute_rmse_with_outliers.R), then run the script [plot_rmse_with_outliers.R](plot_rmse_with_outliers.R).

