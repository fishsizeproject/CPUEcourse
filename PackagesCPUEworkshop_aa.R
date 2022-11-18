
##########################################################
# Packages to install for CPUE workshop
# Carl Smith
##########################################################

# You need to be online for this.


## You might need to install package Rtools. If it is not available from CRAN for the latest version of R, you can download the installer from this page
#https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html

# In R-studio run the following: 
toInstall <- c("anytime", "arm", "car", "DHARMa", "GGally", "ggplot2", "ggpubr", 
               "glmmTMB", "grid", "gridExtra", "lattice", "lawstat", "lme4", 
               "mgcv", "nlme", "outliers", "plotly", "plyr", "tidyverse",
               "scales", "sjlabelled", "sjmisc", "sjPlot", "cellranger","gargle", "timechange", "tzdb", "vroom", "bit")

# Then:
install.packages(toInstall, 
	               dependencies = TRUE, 
	                      repos = "http://cran.us.r-project.org")

# Be patient!


# Install the latest stable version of INLA:
install.packages("INLA", repos=c(getOption("repos"), 
                 INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

# Also install brinla
devtools::install_github("julianfaraway/brinla")

# And install inlatools:
ip <- rownames(installed.packages())
if (!"remotes" %in% ip) {
  install.packages("remotes")}
if (!"INLA" %in% ip) {
  install.packages(
    "INLA", 
    repos = c(getOption("repos"), "https://inla.r-inla-download.org/R/stable"))}
remotes::install_github("inbo/inlatools")

# Then load all the packages:
library(anytime)
library(arm)
library(brinla)
library(car)
library(DHARMa)
library(GGally)
library(ggplot2)
library(ggpubr)
library(glmmTMB)
library(grid)
library(gridExtra)
library(INLA)
library(inlatools)
library(lattice)
library(lawstat)
library(lme4)
library(mgcv)
library(nlme)
library(outliers)
library(plotly)
library(plyr)
library(tidyverse)
library(scales)
library(sjlabelled)
library(sjmisc)
library(sjPlot)

##if loading some of these libraries produces errors like this 
#Error: package or namespace load failed for ‘tidyverse’: .onLoad failed in loadNamespace() for 'readr', details: call: loadNamespace(x) error: there is no package called ‘tzdb’
#you just need to install the missing package separately, using install.package("tzdb") in this specific case 


# If you want to be 100% sure that ggplot2 is working properly,
# then execute the following code.
mydf1 <- data.frame(X = rnorm(100), Y = rnorm(100))
ggplot(mydf1) +
  geom_point(aes(x = X, y = Y)) +
  theme_classic()

#This should produce a scatterplot of Y plotted against X

# If you want to be sure that INLA is working properly,
# then execute the following code.
library(INLA)
set.seed(1966)
mydf2 <- data.frame(X = rnorm(100), Y = rnorm(100))
Itest <- inla(Y ~ X,
              family = "gaussian",
              control.predictor = list(link = 1,
                                       compute = TRUE, 
                                       quantiles = c(0.025, 0.975)),
              data = mydf2)
sum.Itest <- Itest$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
print(sum.Itest, digits = 3)

# This could should return output that looks like this:

#               mean  0.025quant 0.975quant
# (Intercept) 0.1002     -0.103      0.303
# X          -0.0519     -0.252      0.148

############ GOOD LUCK! ################

# ...let me know if you need help or have problems
# if you are having problems - please write to the course repository discussion group on this link 
# you will need a GitHub account to post there, but hey - it is very useful to have one

