# PhenologyTheory
Theory and Bayesian inference of phenological states from biocollection data

The folders and files within provide scripts and data needed to generate the figures for the manuscript: A unified theory and Bayesian framework for phenological inference from biocollection data:
Resolving paradoxes by considering phenophase duration. 

Within each folder, a README file describes how to use the R code to generate the figures. Some of the figures require simulation data. Precomputed simulation data are available in relevant folders, but if reviewers wish to recreate the simulated data, there are subfolders (Simulation, Analysis) that provide additional code to run the simulations.

Some of the simulated data or results of analyses will require additional formatting to match the requirements of the script that generates the figure. The README file again provides additional details, which typically involve sorting uniquely to keep only one header line, reformatting to a tab-separated text vs. csv file, or removing strings such as 'NA' or '_'.

To generate the datasets used for publication, some of the simulations and analyses will take several days to complete on standard desktop computers. Unless the same number of replicates or the same seeds are used, some deviations from the published figures can be expected, although the overall results should not differ from those published, even if different seeds are used when the number of replicates is sufficient. 

The scripts that generate the figures will also carry out the statistical analyses whose results are reported in the MS. The specific outcomes of the statistical analyses will depend on the number of simulation replicates, which can be configured in some of the scripts that carry out these statistical analyses. 

To run the code provided, the following R libraries and their dependencies will be needed for one or more figures, simulations, or analyses:

bayesplot,
cmdstanr,
cowplot,
dplyr,
elevatr,
fitdistrplus,
ggplot2,
here,
latex2exp,
magick,
ncdf4,
phest from github.com/willpearse/phest,
posterior,
purrr,
quantreg,
raster,
readr,
rgdal,
ridigbio,
sn,
sp,
stringr,
tibble,
tidyr,
tidyverse,
viridis

These can be obtained at CRAN (https://cran.r-project.org/) through the standard install.packages R command except for phest, for which a URL is provided above.


