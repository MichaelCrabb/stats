library(testthat)
library(statspackage)

test_that("testMyNCurve", {
  result <- statspackage::myncurve(10, 5, 6)
  expect_equal(result, 0.2118554)
})
