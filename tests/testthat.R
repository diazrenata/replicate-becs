library(testthat)
library(replicatebecs)
test_dir("testthat", reporter = c("check", "progress"))
test_check("replicatebecs")
