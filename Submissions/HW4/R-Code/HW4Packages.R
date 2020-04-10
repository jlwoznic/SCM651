# 
# Course: SCM 651
# Name: Team 73
# HW4 Code: Package Loading
# Due Date: 06/02/2019
# Date Submitted:
#
# Package Section
# ------------------------------------------------------------------

install.packages("plyr", dependencies=TRUE)
library(plyr)

#specify the packages of interest
packages=c("aod", "readxl", "arules",  "arulesviz", "kernlab", "e1071", "gridExtra", "ggplot2", "caret", "CRAN", "zipcodes",
           "stargazer", "gmodels", "pastecs", "Hmisc", "reshape2", "plyr", "plotly", "psych", "maps", "ggmap",
           "dplyr", "lmtest", "car", "neuralnet", "caTools")


#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()

# I find this does not always work, so added to install when required here
install.packages("psych", dependencies = TRUE)
library(psych)
require(dplyr)

install.packages("stargazer", dependencies = TRUE)
library(stargazer)
require(dplyr)

install.packages("caTools")
library(caTools)

install.packages("neuralnet")
library(neuralnet)

install.package ('car')
library(car)


