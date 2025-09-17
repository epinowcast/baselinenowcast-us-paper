library(testthat)
library(baselinenowcastuspaper)

test_results <- test_check("baselinenowcastuspaper")

if (any(as.data.frame(test_results)$warning > 0)) {
  stop("tests failed with warnings", call. = FALSE)
}
