# install.packages("lintr")

library(preModel)
library(devtools)
library(lintr)
library(roxygen2)

setwd("")
install()
install_github("Sokolheavy/preModel")

load_all()

use_testthat()
use_test()

data <- read.csv2("work_data_initial.csv")
data2 <- data[, c(1,3,6,14)]

binneddata <- binning(data2, 2,1)

check()

# code mistakes
lint_package()
