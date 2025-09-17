library(testthat)
library(baselinenowcastpaper)

test_results <- test_check("baselinenowcast-us-paper")

if (any(as.data.frame(test_results)$warning > 0)) {
  stop("tests failed with warnings", call. = FALSE)
}
