library(testthat)
library(replicatebecs)
test_check("replicatebecs")
test_dir("testthat", reporter = c("check", "progress"))
