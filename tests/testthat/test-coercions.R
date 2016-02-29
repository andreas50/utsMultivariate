context("coercions")

test_that("coercions work",{
  # ts
  ts1 <- ts(matrix(1:20, 10, 2), start=c(2016, 1), frequency=12, names=c("apples", "oranges"))
  expect_equal_to_reference(as.uts_vector(ts1), file="test-coercions_ts.rds")
})
