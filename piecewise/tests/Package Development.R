# load packages for development
library(devtools)
library(roxygen2)
library(testthat)



devtools::use_build_ignore("R/hello.R")

devtools::use_build_ignore("tests")

devtools::document()
